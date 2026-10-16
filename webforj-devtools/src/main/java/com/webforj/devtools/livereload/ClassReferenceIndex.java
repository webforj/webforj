package com.webforj.devtools.livereload;

import com.webforj.router.annotation.Route;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;

/**
 * Finds the classes a class reaches through the class references compiled into the application
 * class folders.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class ClassReferenceIndex {

  // The constant pool tag of a class entry, as the class file format defines it.
  private static final int CONSTANT_CLASS_TAG = 7;
  private static final int DECLARATION_FLAGS =
      ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES;
  private static final String CLASS_FILE_SUFFIX = ".class";
  private static final String FILE_PROTOCOL = "file";
  private static final String ROUTE_DESCRIPTOR = Type.getDescriptor(Route.class);

  // The class loader is held weakly and no value may ever refer to it, directly or through a class,
  // or the class loader of a redeployed application could never be collected.
  private final Map<ClassLoader, Map<String, Entry>> entriesByLoader =
      Collections.synchronizedMap(new WeakHashMap<>());

  /**
   * Starts an update of the given changed classes, forgetting what is known about them first.
   *
   * @param loader the class loader the class files are resolved through
   * @param classNames the binary names of the changed classes
   *
   * @return the update
   */
  Update newUpdate(ClassLoader loader, Set<String> classNames) {
    Map<String, Entry> entries =
        entriesByLoader.computeIfAbsent(loader, key -> new ConcurrentHashMap<>());

    // A changed class carries new references, so its entry is dropped by name instead of trusting
    // a file time that a quick recompilation may not have moved.
    entries.keySet().removeAll(classNames);

    return new Update(loader, entries, classNames);
  }

  /**
   * Reads the bytes of a class file.
   *
   * @param path the class file
   *
   * @return the bytes of the class file
   * @throws IOException if the class file cannot be read
   */
  byte[] readClassFile(Path path) throws IOException {
    return Files.readAllBytes(path);
  }

  /**
   * Finds the location of the class file of a class.
   *
   * @param loader the class loader the class file is resolved through
   * @param className the binary name of the class
   *
   * @return the location, empty when the class has no class file
   */
  static Optional<URL> findClassFile(ClassLoader loader, String className) {
    return Optional.ofNullable(loader.getResource(className.replace('.', '/') + CLASS_FILE_SUFFIX));
  }

  /**
   * Converts the location of a class file to the file in a class folder.
   *
   * @param location the location of the class file
   * @param className the binary name of the class
   *
   * @return the class file, empty when the location is not in a class folder
   * @throws UncheckedIOException if the location cannot be converted to a file
   */
  static Optional<Path> toClassFolderFile(URL location, String className) {
    if (!FILE_PROTOCOL.equals(location.getProtocol())) {
      return Optional.empty();
    }

    try {
      return Optional.of(Path.of(location.toURI()));
    } catch (URISyntaxException | IllegalArgumentException e) {
      throw createReadFailure(className, e);
    }
  }

  private static UncheckedIOException createReadFailure(String className, Exception cause) {
    return new UncheckedIOException(new IOException(
        "the class file of " + className + " could not be read, " + cause.getMessage(), cause));
  }

  private static Declarations readDeclarations(byte[] classFile, String className) {
    ClassReader reader = new ClassReader(classFile);
    Declarations declarations = new Declarations();
    Set<String> references = declarations.getReferences();

    // The class entries of the constant pool name every class the code creates, calls, or reads.
    char[] buffer = new char[reader.getMaxStringLength()];
    for (int index = 1; index < reader.getItemCount(); index++) {
      // The second slot of a long or a double has no entry and no offset.
      int offset = reader.getItem(index);
      if (offset > 0 && reader.readByte(offset - 1) == CONSTANT_CLASS_TAG) {
        addType(references, Type.getObjectType(reader.readUTF8(offset, buffer)));
      }
    }

    // A class that only types a field, a parameter, or a type argument, an injected dependency or
    // a list of components for example, has no class entry, so the declarations are read as well,
    // without the method code.
    reader.accept(new DeclarationVisitor(declarations), DECLARATION_FLAGS);
    references.remove(className);

    return declarations;
  }

  private static void addType(Set<String> references, Type type) {
    Type element = type;
    while (element.getSort() == Type.ARRAY) {
      element = element.getElementType();
    }

    if (element.getSort() == Type.OBJECT) {
      references.add(element.getClassName());
    }
  }

  /**
   * Reads the class files of one class update, each one at most once.
   *
   * <p>
   * The changed classes are read again whatever their file time says. A class read by an earlier
   * update is reused while its class file keeps its file time.
   * </p>
   *
   * @since 26.03
   */
  final class Update {
    private final ClassLoader loader;
    private final Map<String, Entry> entries;
    private final Set<String> classNames;

    private Update(ClassLoader loader, Map<String, Entry> entries, Set<String> classNames) {
      this.loader = loader;
      this.entries = entries;
      this.classNames = classNames;
    }

    /**
     * Returns the classes the class file of the given class references.
     *
     * @param className the binary name of the class
     *
     * @return the binary names of the referenced classes, empty when the class has no class file in
     *         a class folder
     * @throws UncheckedIOException if the class file cannot be read
     */
    Set<String> getReferences(String className) {
      Entry entry = getEntry(className);
      return entry != null ? entry.getReferences() : Collections.emptySet();
    }

    /**
     * Indicates whether the class file of the given class carries the {@link Route} annotation.
     *
     * @param className the binary name of the class
     *
     * @return {@code true} when the class file in a class folder carries the annotation
     * @throws UncheckedIOException if the class file cannot be read
     */
    boolean isRoute(String className) {
      Entry entry = getEntry(className);
      return entry != null && entry.isRoute();
    }

    /**
     * Starts a walk that looks for the changed classes of this update.
     *
     * <p>
     * A boundary class is neither reached nor followed, so a walk never crosses into a class that
     * is created by something other than the code referencing it. A changed class that is also a
     * boundary is therefore never looked for, and the walk is complete from the start when every
     * changed class is one.
     * </p>
     *
     * @param boundaries the binary names of the classes the walk stops at
     *
     * @return the walk
     */
    Walk newWalk(Set<String> boundaries) {
      Set<String> targets = new HashSet<>(classNames);
      targets.removeAll(boundaries);

      return new Walk(this, targets, boundaries);
    }

    private Entry getEntry(String className) {
      Entry entry = entries.get(className);
      if (entry != null && entry.isCurrent()) {
        return entry;
      }

      Entry read = readEntry(className);
      if (read == null) {
        // The stale entry of a class file that disappeared is dropped, so a later walk stops paying
        // for a file time that can no longer be read.
        entries.remove(className);

        return null;
      }

      entries.put(className, read);

      return read;
    }

    private Entry readEntry(String className) {
      Optional<URL> location = findClassFile(loader, className);
      if (location.isEmpty()) {
        // A class without a class file, a missing or generated one, is not remembered, so the file
        // is looked up again once it exists.
        return null;
      }

      Optional<Path> path = toClassFolderFile(location.get(), className);
      if (path.isEmpty()) {
        // Library and platform classes never change while the application runs, so their
        // references are never followed and the empty answer is kept for good.
        return new Entry(null, null, new Declarations());
      }

      try {
        FileTime modified = Files.getLastModifiedTime(path.get());
        Declarations declarations = readDeclarations(readClassFile(path.get()), className);

        return new Entry(path.get(), modified, declarations);
      } catch (IOException | RuntimeException e) {
        throw createReadFailure(className, e);
      }
    }
  }

  /**
   * Walks the references from one origin after another, visiting every class once across the
   * origins.
   *
   * <p>
   * The origins are handed over root first, so a class the walk has visited from an earlier origin
   * has already given up everything it reaches, and a later origin never reaches it again.
   * </p>
   */
  static final class Walk {
    private final Update update;
    private final Set<String> targets;
    private final Set<String> boundaries;
    private final Set<String> visited = new HashSet<>();
    private final Set<String> reached = new HashSet<>();

    private Walk(Update update, Set<String> targets, Set<String> boundaries) {
      this.update = update;
      this.targets = targets;
      this.boundaries = boundaries;
    }

    /**
     * Reaches from the given origin, the origin itself included.
     *
     * @param origin the binary name of the class the walk continues from
     *
     * @return the changed classes this origin is the first to reach
     * @throws UncheckedIOException if a class file in a class folder cannot be read
     */
    Set<String> reach(String origin) {
      Set<String> found = new HashSet<>();
      if (isComplete() || !visited.add(origin)) {
        return found;
      }

      // The origin is walked even when it is a boundary, the other boundaries are not.
      Deque<String> pending = new ArrayDeque<>();
      visit(origin, found, pending);
      while (!pending.isEmpty() && !isComplete()) {
        for (String reference : update.getReferences(pending.pop())) {
          if (!boundaries.contains(reference) && visited.add(reference)) {
            visit(reference, found, pending);
          }
        }
      }

      return found;
    }

    /**
     * Indicates whether every changed class that is not a boundary has been reached.
     *
     * @return {@code true} when nothing is left to look for
     */
    boolean isComplete() {
      return reached.size() == targets.size();
    }

    /**
     * Returns the changed classes no origin has reached, in name order.
     *
     * @return the unreached classes
     */
    Set<String> getUnreached() {
      Set<String> unreached = new TreeSet<>(targets);
      unreached.removeAll(reached);

      return unreached;
    }

    private void visit(String className, Set<String> found, Deque<String> pending) {
      if (targets.contains(className)) {
        reached.add(className);
        found.add(className);
      }

      pending.push(className);
    }
  }

  private static final class DeclarationVisitor extends ClassVisitor {
    private final Declarations declarations;

    DeclarationVisitor(Declarations declarations) {
      super(Opcodes.ASM9);
      this.declarations = declarations;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AnnotationVisitor visitAnnotation(String descriptor, boolean visible) {
      if (ROUTE_DESCRIPTOR.equals(descriptor)) {
        declarations.setRoute(true);
      }

      return null;
    }

    @Override
    public FieldVisitor visitField(int access, String name, String descriptor, String signature,
        Object value) {
      addType(declarations.getReferences(), Type.getType(descriptor));
      addSignatureTypes(signature);

      return null;
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature,
        String[] exceptions) {
      addType(declarations.getReferences(), Type.getReturnType(descriptor));
      for (Type argument : Type.getArgumentTypes(descriptor)) {
        addType(declarations.getReferences(), argument);
      }

      addSignatureTypes(signature);

      return null;
    }

    private void addSignatureTypes(String signature) {
      // The generic signature is present only for a declaration with type arguments, and the
      // descriptor already carries the erased types.
      if (signature != null) {
        new SignatureReader(signature)
            .accept(new SignatureTypeVisitor(declarations.getReferences()));
      }
    }
  }

  private static final class SignatureTypeVisitor extends SignatureVisitor {
    private final Set<String> references;
    private String current;

    SignatureTypeVisitor(Set<String> references) {
      super(Opcodes.ASM9);
      this.references = references;
    }

    @Override
    public void visitClassType(String name) {
      current = name.replace('/', '.');
      references.add(current);
    }

    @Override
    public void visitInnerClassType(String name) {
      // The reader hands only the simple name of an inner class, after its outer class.
      current = current + "$" + name;
      references.add(current);
    }

    @Override
    public SignatureVisitor visitTypeArgument(char wildcard) {
      // A type argument gets its own visitor, so its class never displaces the enclosing one.
      return new SignatureTypeVisitor(references);
    }
  }

  private static final class Declarations {
    private final Set<String> references = new HashSet<>();
    private boolean route;

    Set<String> getReferences() {
      return references;
    }

    boolean isRoute() {
      return route;
    }

    void setRoute(boolean route) {
      this.route = route;
    }
  }

  private static final class Entry {
    private final Path path;
    private final FileTime modified;
    private final Declarations declarations;

    Entry(Path path, FileTime modified, Declarations declarations) {
      this.path = path;
      this.modified = modified;
      this.declarations = declarations;
    }

    Set<String> getReferences() {
      return declarations.getReferences();
    }

    boolean isRoute() {
      return declarations.isRoute();
    }

    boolean isCurrent() {
      if (path == null) {
        return true;
      }

      try {
        return modified.equals(Files.getLastModifiedTime(path));
      } catch (IOException e) {
        return false;
      }
    }
  }
}
