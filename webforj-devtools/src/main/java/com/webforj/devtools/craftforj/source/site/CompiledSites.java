package com.webforj.devtools.craftforj.source.site;

import com.webforj.component.ComponentSourceRegistry.SourceFrame;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import com.webforj.devtools.craftforj.source.site.model.CreationSite;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * Reads the creation sites out of the classes the application runs.
 *
 * <p>
 * The frame recorded while a component was created names one instruction of one method. The
 * compiled class tells what that instruction is, a creation or a call, and where it stands among
 * the instructions of the same kind and name in the method.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class CompiledSites {

  private static final String CONSTRUCTOR = "<init>";
  private static final String FILE_PROTOCOL = "file";

  // A class is read once for every time it was compiled. The values hold names and numbers only,
  // never a class or a class loader.
  private static final Map<String, CompiledClass> CLASSES = new ConcurrentHashMap<>();

  private CompiledSites() {}

  /**
   * Finds the site a recorded frame stands for in the class the application loaded.
   *
   * @param frame the frame recorded while the component was created
   * @param createdAt the time the component was created at, in milliseconds since the epoch
   *
   * @return the site, or empty when the frame carries no instruction or its instruction neither
   *         creates an object nor calls a method
   *
   * @throws SourceModificationException when the class cannot be read or was compiled again after
   *         the component was created
   */
  static Optional<CreationSite> find(SourceFrame frame, long createdAt) {
    if (!hasInstruction(frame)) {
      return Optional.empty();
    }

    String className = frame.getSourcePoint().className();
    CompiledClass compiled = read(className);
    if (compiled.modifiedAt > createdAt) {
      throw createStale(className);
    }

    return findIn(compiled, frame);
  }

  /**
   * Checks whether the class the application loaded holds the instruction of a recorded frame.
   *
   * @param frame the frame recorded while a component was created
   * @param createdAt the time that component was created at, in milliseconds since the epoch
   *
   * @return {@code false} when the class was compiled before the component was created and holds no
   *         creation and no call where the frame says, {@code true} otherwise
   *
   * @throws SourceModificationException when the class cannot be read
   */
  static boolean isKnown(SourceFrame frame, long createdAt) {
    if (!hasInstruction(frame)) {
      return false;
    }

    CompiledClass compiled = read(frame.getSourcePoint().className());
    // A frame recorded before the class was compiled again tells nothing about the class
    if (compiled.modifiedAt > createdAt) {
      return true;
    }

    CompiledMethod method = compiled.methods.get(frame.getMethodName() + frame.getDescriptor());
    Integer line = method == null ? null : method.lines.get(frame.getBytecodeIndex());

    return line != null && line == frame.getSourcePoint().lineNumber()
        && method.findInstruction(frame.getBytecodeIndex()) != null;
  }

  private static Optional<CreationSite> findIn(CompiledClass compiled, SourceFrame frame) {
    SourcePoint point = frame.getSourcePoint();
    CompiledMethod method = compiled.methods.get(frame.getMethodName() + frame.getDescriptor());
    Integer line = method == null ? null : method.lines.get(frame.getBytecodeIndex());
    if (line == null || line != point.lineNumber()) {
      throw createStale(point.className());
    }

    Instruction target = method.findInstruction(frame.getBytecodeIndex());
    if (target == null) {
      return Optional.empty();
    }

    List<Instruction> same = method.instructions.stream()
        .filter(
            instruction -> instruction.kind == target.kind && instruction.name.equals(target.name))
        .toList();
    CreationSite.Kind kind = method.bridge ? CreationSite.Kind.BRIDGE : target.kind;

    return Optional.of(CreationSite.builder().setClassName(point.className())
        .setMethod(frame.getMethodName(), frame.getDescriptor()).setKind(kind).setName(target.name)
        .setProducedType(target.producedType).setPosition(same.indexOf(target), same.size())
        .setLine(point.lineNumber()).build());
  }

  private static boolean hasInstruction(SourceFrame frame) {
    return frame.getMethodName() != null && frame.getDescriptor() != null
        && frame.getBytecodeIndex() >= 0;
  }

  private static SourceModificationException createStale(String className) {
    return new SourceModificationException(getSimpleName(className.replace('.', '/'))
        + " was compiled again after this component was created, reload the application");
  }

  private static CompiledClass read(String className) {
    Class<?> type = TypeResolver.load(className);
    String resource = className.replace('.', '/') + ".class";
    URL location = type == null || type.getClassLoader() == null ? null
        : type.getClassLoader().getResource(resource);
    if (location == null) {
      throw new SourceModificationException("The class " + className + " cannot be read");
    }

    try {
      long modifiedAt = FILE_PROTOCOL.equals(location.getProtocol())
          ? Files.getLastModifiedTime(Path.of(location.toURI())).toMillis()
          : 0;
      CompiledClass known = CLASSES.get(location.toString());
      if (known != null && known.modifiedAt == modifiedAt) {
        return known;
      }

      try (InputStream content = location.openStream()) {
        CompiledClass compiled = parse(className, content.readAllBytes(), modifiedAt);
        CLASSES.put(location.toString(), compiled);

        return compiled;
      }
    } catch (IOException | URISyntaxException e) {
      throw new SourceModificationException(
          "The class " + className + " cannot be read, " + e.getMessage());
    }
  }

  private static CompiledClass parse(String className, byte[] classFile, long modifiedAt) {
    try {
      OffsetReader reader = new OffsetReader(classFile);
      CompiledClassVisitor scanner = new CompiledClassVisitor(reader);
      reader.accept(scanner, ClassReader.SKIP_FRAMES);

      return new CompiledClass(modifiedAt, scanner.methods);
    } catch (RuntimeException e) {
      // The reader reports what is no class file, or only a part of one, in more than one way
      throw new SourceModificationException("The class " + className + " cannot be read");
    }
  }

  // The name a class has in source, the digits the compiler puts in front of a local class and
  // the ones that stand for an anonymous class are not part of it
  private static String getSimpleName(String internalName) {
    String name = internalName.substring(internalName.lastIndexOf('/') + 1);
    name = name.substring(name.lastIndexOf('$') + 1);
    int start = 0;
    while (start < name.length() && Character.isDigit(name.charAt(start))) {
      start++;
    }

    return name.substring(start);
  }

  /**
   * One instruction that creates an object or calls a method.
   */
  private static final class Instruction {

    private final int offset;
    private final CreationSite.Kind kind;
    private final String name;
    private final String producedType;

    Instruction(int offset, CreationSite.Kind kind, String name, String producedType) {
      this.offset = offset;
      this.kind = kind;
      this.name = name;
      this.producedType = producedType;
    }
  }

  /**
   * What one method of a class runs.
   */
  private static final class CompiledMethod {

    private final boolean bridge;
    private final List<Instruction> instructions = new ArrayList<>();
    private final Map<Integer, Integer> lines = new HashMap<>();

    CompiledMethod(boolean bridge) {
      this.bridge = bridge;
    }

    Instruction findInstruction(int offset) {
      return instructions.stream().filter(instruction -> instruction.offset == offset).findFirst()
          .orElse(null);
    }
  }

  /**
   * The methods of a class, as the file of a given time holds them.
   */
  private static final class CompiledClass {

    private final long modifiedAt;
    private final Map<String, CompiledMethod> methods;

    CompiledClass(long modifiedAt, Map<String, CompiledMethod> methods) {
      this.modifiedAt = modifiedAt;
      this.methods = methods;
    }
  }

  /**
   * A reader that tells where in the code of a method every instruction starts.
   */
  private static final class OffsetReader extends ClassReader {

    private final Map<Label, Integer> offsets = new HashMap<>();
    private Label[] filled;

    OffsetReader(byte[] classFile) {
      super(classFile);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Label readLabel(int bytecodeOffset, Label[] labels) {
      // The reader hands a label to the visitor right before the instruction at its offset, and
      // asks for labels only where the code is jumped to or a line starts. A label at every
      // offset makes it tell where each instruction starts.
      if (labels != filled) {
        filled = labels;
        for (int offset = 0; offset < labels.length; offset++) {
          offsets.put(super.readLabel(offset, labels), offset);
        }
      }

      return super.readLabel(bytecodeOffset, labels);
    }

    int getOffset(Label label) {
      return offsets.getOrDefault(label, -1);
    }
  }

  /**
   * Collects what every method of a class runs.
   */
  private static final class CompiledClassVisitor extends ClassVisitor {

    private final OffsetReader reader;
    private final Map<String, CompiledMethod> methods = new HashMap<>();

    CompiledClassVisitor(OffsetReader reader) {
      super(Opcodes.ASM9);
      this.reader = reader;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature,
        String[] exceptions) {
      CompiledMethod method = new CompiledMethod((access & Opcodes.ACC_BRIDGE) != 0);
      methods.put(name + descriptor, method);

      return new CompiledMethodVisitor(reader, method);
    }
  }

  /**
   * Collects the creations and the calls of one method, and the line of every instruction.
   */
  private static final class CompiledMethodVisitor extends MethodVisitor {

    private final OffsetReader reader;
    private final CompiledMethod method;
    private final Deque<String> created = new ArrayDeque<>();
    private int offset = -1;
    private int line = -1;

    CompiledMethodVisitor(OffsetReader reader, CompiledMethod method) {
      super(Opcodes.ASM9);
      this.reader = reader;
      this.method = method;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitLabel(Label label) {
      offset = reader.getOffset(label);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitLineNumber(int number, Label start) {
      line = number;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitInsn(int opcode) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitIntInsn(int opcode, int operand) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitVarInsn(int opcode, int varIndex) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitTypeInsn(int opcode, String type) {
      if (opcode == Opcodes.NEW) {
        created.push(type);
      }

      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitFieldInsn(int opcode, String owner, String name, String descriptor) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitMethodInsn(int opcode, String owner, String name, String descriptor,
        boolean isInterface) {
      visitInstruction(createInstruction(owner, name, descriptor));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitInvokeDynamicInsn(String name, String descriptor, Handle bootstrap,
        Object... arguments) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitJumpInsn(int opcode, Label label) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitLdcInsn(Object value) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitIincInsn(int varIndex, int increment) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitTableSwitchInsn(int min, int max, Label otherwise, Label... labels) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitLookupSwitchInsn(Label otherwise, int[] keys, Label[] labels) {
      visitInstruction(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visitMultiANewArrayInsn(String descriptor, int dimensions) {
      visitInstruction(null);
    }

    // The label in front of an instruction tells its offset, which holds for that one
    // instruction only
    private void visitInstruction(Instruction instruction) {
      if (offset >= 0) {
        method.lines.put(offset, line);
      }

      if (instruction != null) {
        method.instructions.add(instruction);
      }

      offset = -1;
    }

    private Instruction createInstruction(String owner, String name, String descriptor) {
      if (!CONSTRUCTOR.equals(name)) {
        Type returned = Type.getReturnType(descriptor);
        String produced = returned.getSort() == Type.OBJECT ? returned.getClassName() : null;

        return new Instruction(offset, CreationSite.Kind.CALL, name, produced);
      }

      // A constructor call that initializes no object created before it hands over to super or
      // this
      if (created.isEmpty() || !created.peek().equals(owner)) {
        return new Instruction(offset, CreationSite.Kind.DELEGATION, "", owner.replace('/', '.'));
      }

      created.pop();

      return new Instruction(offset, CreationSite.Kind.CREATION, getSimpleName(owner),
          owner.replace('/', '.'));
    }
  }
}
