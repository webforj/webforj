package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ParseResult;
import com.github.javaparser.Position;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.printer.lexicalpreservation.LexicalPreservingPrinter;
import com.webforj.devtools.craftforj.source.SourceFileEditor;
import com.webforj.devtools.craftforj.source.SourceImports;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.staging.CompileValidator;
import com.webforj.devtools.craftforj.source.staging.SourceHasher;
import com.webforj.devtools.craftforj.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.source.staging.model.CompileDiagnostic;
import com.webforj.devtools.craftforj.source.staging.model.StagedFile;
import com.webforj.devtools.craftforj.source.staging.model.ValidationResult;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import com.webforj.devtools.craftforj.source.structure.model.ComponentCreation;
import com.webforj.devtools.craftforj.source.structure.model.InsertResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Changes the component structure of Java source.
 *
 * <p>
 * The editor creates a component at an attach point, removes a component with everything below it,
 * and moves a component to another place, another parent or another file. It works on Java shapes
 * and knows no component by name. Every change is worked out on all files first and written only
 * when each of them succeeded, so a refusal leaves the disk as it was.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class StructureModifier {

  private final SourceFileEditor fileEditor;
  private final SourceParserService parserService;
  private final CompileValidator compileValidator;

  /**
   * Creates an editor.
   *
   * @param fileEditor the file editor
   * @param parserService the parser service
   */
  public StructureModifier(SourceFileEditor fileEditor, SourceParserService parserService) {
    this.fileEditor = fileEditor;
    this.parserService = parserService;
    this.compileValidator = new CompileValidator();
  }

  /**
   * Removes a component and the components below it.
   *
   * <p>
   * A component the rest of the file still reads keeps its declaration and is only taken out of
   * what it is attached to. A component below the removed one that is still read stays as it is.
   * </p>
   *
   * @param components the component first, then every component below it that the same file creates
   * @param from the place the component is attached at, or {@code null} when it is not known
   * @param dryRun {@code true} to produce the patches without writing
   *
   * @return the patches of the files that change
   *
   * @throws IOException when a file cannot be read
   * @throws SourceModificationException when the change is refused
   */
  public List<FilePatch> remove(List<SourceLocation> components, AttachPoint from, boolean dryRun)
      throws IOException {
    SourceLocation root = components.get(0);
    if (from != null) {
      requireSameFile(root, from.getParent());
    }

    SourceImports imports = new SourceImports();
    FilePatch patch = fileEditor.edit(Path.of(root.getFile()), imports, true, cu -> {
      cut(cu, findSlices(cu, components, false), from, imports, true);

      return true;
    });

    if (!dryRun) {
      requireCompiles(patch);
    }

    return commit(List.of(patch), dryRun);
  }

  /**
   * Creates a component and attaches it.
   *
   * @param creation the source the component is created from
   * @param to the place the component is attached at
   * @param dryRun {@code true} to produce the patches without writing
   *
   * @return the patches of the files that change and where the new component is created
   *
   * @throws IOException when a file cannot be read
   * @throws SourceModificationException when the change is refused
   */
  InsertResult insert(ComponentCreation creation, AttachPoint to, boolean dryRun)
      throws IOException {
    SourceImports imports = new SourceImports();
    imports.getRequired().add(creation.getType());
    imports.getRequired().addAll(creation.getImports());

    Created created = new Created();
    FilePatch patch = fileEditor.edit(Path.of(to.getParent().getFile()), imports, true, cu -> {
      Expression node = writeNew(cu, creation, to, created);
      created.kind = node.getClass();
      created.text = node.toString();
      created.index = AstFinder
          .indexOfSame(cu.findAll(node.getClass(), n -> n.toString().equals(created.text)), node);

      return true;
    });

    List<FilePatch> files = commit(List.of(patch), dryRun);

    return new InsertResult(files, locate(patch, creation, created));
  }

  /**
   * Tells whether the parent already attaches something with one of the point's methods.
   *
   * <p>
   * A call counts when it is made on the parent itself, through its variable, {@code this.name}, a
   * fluent chain that starts at it, or {@code getBoundComponent()} in a composite. A call on
   * another component of the same file does not.
   * </p>
   *
   * @param point the parent and the methods of the slot
   *
   * @return {@code true} when the parent calls one of the methods
   *
   * @throws IOException when the file cannot be read
   * @throws SourceModificationException when the parent cannot be found in the file
   */
  boolean hasAttachCall(AttachPoint point) throws IOException {
    CompilationUnit cu = parserService.parse(Path.of(point.getParent().getFile())).orElse(null);
    if (cu == null) {
      return false;
    }

    ParentReference parent = ParentReference.of(cu, point.getParent());

    return cu.findAll(MethodCallExpr.class).stream()
        .anyMatch(call -> point.getMethodNames().contains(call.getNameAsString())
            && parent.isCallOnParent(call));
  }

  /**
   * Adds a part to a parent through a call, a tab, a list item, a column.
   *
   * @param to the parent and the method that takes the part
   * @param arguments the argument sources of the call
   * @param imports the classes the arguments need imported
   * @param dryRun {@code true} to produce the patches without writing
   *
   * @return the patches of the files that change
   *
   * @throws IOException when a file cannot be read
   * @throws SourceModificationException when the change is refused
   */
  List<FilePatch> insertCall(AttachPoint to, List<String> arguments, Set<String> imports,
      boolean dryRun) throws IOException {
    SourceImports required = new SourceImports();
    required.getRequired().addAll(imports);

    FilePatch patch = fileEditor.edit(Path.of(to.getParent().getFile()), required, true, cu -> {
      ParentReference parent = ParentReference.of(cu, to.getParent());
      List<Expression> parsed = new ArrayList<>();
      for (String argument : arguments) {
        parsed.add(parseExpression(to.getMethodName(), argument));
      }
      new AttachWriter(cu, parent, to).placeCall(parsed);

      return true;
    });

    return commit(List.of(patch), dryRun);
  }

  /**
   * Moves a component, with the components below it, to another place.
   *
   * @param components the component first, then every component below it that the same file creates
   * @param from the place the component is attached at
   * @param to the place the component goes to, in the same file or in another one
   * @param dryRun {@code true} to produce the patches without writing
   *
   * @return the patches of the files that change
   *
   * @throws IOException when a file cannot be read
   * @throws SourceModificationException when the change is refused
   */
  List<FilePatch> move(List<SourceLocation> components, AttachPoint from, AttachPoint to,
      boolean dryRun) throws IOException {
    SourceLocation root = components.get(0);
    requireSameFile(root, from.getParent());
    requireOutside(components, to);

    Path source = Path.of(root.getFile());
    Path target = Path.of(to.getParent().getFile());
    if (isSameFile(source, target)) {
      SourceImports imports = new SourceImports();
      FilePatch patch = fileEditor.edit(source, imports, true, cu -> {
        moveWithin(cu, components, from, to);

        return true;
      });

      return commit(List.of(patch), dryRun);
    }

    Transfer transfer = new Transfer();
    Map<String, String> renames = findRenames(components, to);
    SourceImports sourceImports = new SourceImports();
    FilePatch cutPatch = fileEditor.edit(source, sourceImports, true, cu -> {
      List<ComponentSlice> slices = findSlices(cu, components, true);
      DetachWriter.rename(slices, renames);
      transfer.source = cu;
      transfer.names = getNames(components, renames);
      transfer.result = cut(cu, slices, from, sourceImports, false);

      return true;
    });

    SourceImports targetImports = new SourceImports();
    FilePatch pastePatch = fileEditor.edit(target, targetImports, true, cu -> {
      SliceDependencies dependencies =
          new SliceDependencies(transfer.source, transfer.result.getNodes(), transfer.names);
      dependencies.requireSelfContained(false, Set.of(), source.getFileName().toString());
      targetImports.getRequired().addAll(dependencies.resolveImports(cu));
      paste(cu, transfer, to);

      return true;
    });

    return commit(List.of(cutPatch, pastePatch), dryRun);
  }

  private Expression writeNew(CompilationUnit cu, ComponentCreation creation, AttachPoint to,
      Created created) {
    ParentReference parent = ParentReference.of(cu, to.getParent());
    AttachWriter writer = new AttachWriter(cu, parent, to);
    ClassOrInterfaceDeclaration type = findClass(cu, parent);
    String name =
        AstModifier.generateFreeVariableName(decapitalize(creation.getSimpleType()), type);

    VariableDeclarator sibling = findSibling(cu, parent, to);
    if (sibling == null && creation.getCalls().isEmpty() && isInlineStyle(cu, parent, to)) {
      Expression inline = parseExpression(creation);
      writer.place(inline, List.of(), List.of());

      return inline;
    }

    List<Statement> leading = new ArrayList<>();
    Statement declaration = parseStatement(creation,
        creation.getSimpleType() + " " + name + " = " + creation.getExpression() + ";");
    Expression expression;

    if (sibling != null && sibling.getParentNode().orElse(null) instanceof FieldDeclaration field) {
      VariableDeclarator declarator = declaration.asExpressionStmt().getExpression()
          .asVariableDeclarationExpr().getVariable(0).clone();
      boolean initialized = sibling.getInitializer().isPresent();
      if (initialized) {
        expression = declarator.getInitializer().orElseThrow();
      } else {
        Statement assign = parseStatement(creation, name + " = " + creation.getExpression() + ";");
        expression = assign.asExpressionStmt().getExpression().asAssignExpr().getValue();
        leading.add(assign);
        declarator.removeInitializer();
      }

      FieldDeclaration declared =
          new FieldDeclaration(new NodeList<>(Modifier.privateModifier()), declarator);
      ClassOrInterfaceDeclaration owner =
          AstFinder.findAncestor(field, ClassOrInterfaceDeclaration.class).orElseThrow();
      owner.getMembers().addAfter(declared, field);
    } else {
      expression = declaration.asExpressionStmt().getExpression().asVariableDeclarationExpr()
          .getVariable(0).getInitializer().orElseThrow();
      leading.add(declaration);
    }

    for (String call : creation.getCalls()) {
      leading.add(parseStatement(creation, name + "." + call + ";"));
    }

    writer.place(new NameExpr(name), leading, List.of());
    created.name = name;

    return expression;
  }

  // The patched text is parsed again, the new creation is the node with the same text there
  private SourceLocation locate(FilePatch patch, ComponentCreation creation, Created created) {
    if (patch.getPatched() == null || created.kind == null || created.index < 0) {
      return null;
    }

    CompilationUnit cu = parserService.parse(patch.getPatched()).orElse(null);
    if (cu == null) {
      return null;
    }

    List<? extends Node> nodes = cu.findAll(created.kind, n -> n.toString().equals(created.text));
    if (created.index >= nodes.size()) {
      return null;
    }

    Node node = nodes.get(created.index);
    Integer line = node.getRange().map(range -> range.begin.line).orElse(null);

    return new SourceLocation(patch.getFile(), line, getDeclaringClass(cu, node), created.name,
        creation.getType());
  }

  private static String getDeclaringClass(CompilationUnit cu, Node node) {
    TypeDeclaration<?> outer = null;
    Node current = node;
    do {
      if (current instanceof TypeDeclaration<?> type) {
        outer = type;
      }

      current = current.getParentNode().orElse(null);
    } while (current != null);

    if (outer == null) {
      return null;
    }

    return cu.getPackageDeclaration().map(pkg -> pkg.getNameAsString() + ".").orElse("")
        + outer.getNameAsString();
  }

  private CutResult cut(CompilationUnit cu, List<ComponentSlice> slices, AttachPoint from,
      SourceImports imports, boolean keepReferenced) {
    ParentReference parent = from == null ? null : ParentReference.of(cu, from.getParent());
    AttachWriter writer = from == null ? null : new AttachWriter(cu, parent, from);

    CutResult result = new DetachWriter(parent, writer).cut(slices, null, keepReferenced);
    new SliceDependencies(cu, result.getNodes(), Set.of()).trackImports(imports);

    return result;
  }

  private void moveWithin(CompilationUnit cu, List<SourceLocation> components, AttachPoint from,
      AttachPoint to) {
    List<ComponentSlice> slices = findSlices(cu, components, true);
    ComponentSlice root = slices.get(0);
    ParentReference fromParent = ParentReference.of(cu, from.getParent());
    ParentReference toParent = ParentReference.of(cu, to.getParent());
    AttachWriter fromWriter = new AttachWriter(cu, fromParent, from);
    AttachWriter toWriter = new AttachWriter(cu, toParent, to);
    DetachWriter cutter = new DetachWriter(fromParent, fromWriter);

    Expression attachment = fromWriter.findAttachment(root);
    AttachWriter.requireUnconditional(attachment,
        "The attach call of " + ComponentSlice.describe(root.getLocation()));

    if (to.getAnchor() != null && isSame(to.getAnchor(), root.getLocation())) {
      throw new SourceModificationException("A component cannot be placed next to itself");
    }

    boolean sameParent = isSame(from.getParent(), to.getParent());
    if (root.getInlineCreation() != null) {
      Expression inline = DetachWriter.copy(root.getInlineCreation());
      cutter.detach(root, true);
      toWriter.place(inline, List.of(), List.of());

      return;
    }

    Position pivot = attachment.getRange().map(range -> range.begin).orElse(null);
    List<Statement> itemCalls = cutter.detach(root, !sameParent);
    NameExpr child = new NameExpr(root.getVariableName());
    Statement attached = toWriter.place(child, List.of(), List.of());

    if (!isVisible(slices, attached)) {
      ClassOrInterfaceDeclaration owner = AstFinder
          .findAncestor(root.getDeclarator(), ClassOrInterfaceDeclaration.class).orElseThrow();
      Map<String, String> renames = findRenames(components, root, attached);
      DetachWriter.rename(slices, renames);
      child.setName(renames.getOrDefault(child.getNameAsString(), child.getNameAsString()));
      relocate(cu, owner, cutter.cut(slices, pivot, false), getNames(components, renames), child);
    }

    followAttach(itemCalls, attached);
  }

  // A local declared below the new attach call, or in another method, travels to it
  private void relocate(CompilationUnit cu, ClassOrInterfaceDeclaration from, CutResult result,
      Set<String> names, NameExpr child) {
    Statement attached = AttachWriter.findBlockStatement(child);
    ClassOrInterfaceDeclaration into =
        AstFinder.findAncestor(attached, ClassOrInterfaceDeclaration.class).orElseThrow();

    new SliceDependencies(cu, result.getNodes(), names).requireSelfContained(from == into,
        getVisibleNames(attached), cu.getStorage().map(CompilationUnit.Storage::getFileName)
            .orElse(from.getNameAsString() + ".java"));

    BlockStmt block = (BlockStmt) attached.getParentNode().orElseThrow();
    int index = block.getStatements().indexOf(attached);
    for (Statement statement : result.getLeading()) {
      AttachWriter.addStatement(block, index++, statement);
    }

    index++;
    for (Statement statement : result.getTrailing()) {
      AttachWriter.addStatement(block, index++, statement);
    }

    result.getFields().forEach(field -> addField(into, field));
  }

  private void paste(CompilationUnit cu, Transfer transfer, AttachPoint to) {
    ParentReference parent = ParentReference.of(cu, to.getParent());
    AttachWriter writer = new AttachWriter(cu, parent, to);
    ClassOrInterfaceDeclaration type = findClass(cu, parent);
    CutResult result = transfer.result;

    // What an inline component holds may be declared before it, and travels with it
    result.getFields().forEach(field -> addField(type, field));
    Expression child = result.getInlineCreation() != null ? result.getInlineCreation()
        : new NameExpr(transfer.names.iterator().next());
    writer.place(child, result.getLeading(), result.getTrailing());
  }

  // The names are settled before the cut, the renamed source then prints as it was written
  private Map<String, String> findRenames(List<SourceLocation> components, AttachPoint to)
      throws IOException {
    ParseResult<CompilationUnit> parsed =
        parserService.parseWithProblems(Files.readString(Path.of(to.getParent().getFile())));
    CompilationUnit cu = parsed.isSuccessful() ? parsed.getResult().orElse(null) : null;
    if (cu == null) {
      return Map.of();
    }

    ClassOrInterfaceDeclaration type = findClass(cu, ParentReference.of(cu, to.getParent()));
    Map<String, String> renames = new LinkedHashMap<>();
    Set<String> taken = new LinkedHashSet<>();
    for (String name : getNames(components, Map.of())) {
      String free = AstModifier.generateFreeVariableName(name, type);
      while (!taken.add(free)) {
        free = AstModifier.generateFreeVariableName(free + "2", type);
      }

      if (!free.equals(name)) {
        renames.put(name, free);
      }
    }

    return renames;
  }

  private static Map<String, String> findRenames(List<SourceLocation> components,
      ComponentSlice root, Statement attached) {
    CallableDeclaration<?> callable = findCallable(attached);
    if (callable == null || callable == findCallable(root.getDeclarator())) {
      return Map.of();
    }

    BlockStmt body = callable.findFirst(BlockStmt.class).orElseThrow();
    Map<String, String> renames = new LinkedHashMap<>();
    for (String name : getNames(components, Map.of())) {
      String free = AstModifier.generateFreeVariableName(name, body);
      if (!free.equals(name)) {
        renames.put(name, free);
      }
    }

    return renames;
  }

  private List<FilePatch> commit(List<FilePatch> patches, boolean dryRun) {
    List<FilePatch> changed = patches.stream().filter(patch -> patch.getPatched() != null).toList();
    if (dryRun || changed.isEmpty()) {
      return changed;
    }

    Map<String, StagedFile> staged = new LinkedHashMap<>();
    SourceStagingArea staging = new SourceStagingArea(() -> staged);
    for (FilePatch patch : changed) {
      staging.stage(new StagedFile(patch.getFile(), SourceHasher.hash(patch.getOriginal()),
          patch.getPatched(), false, false));
    }

    staging.apply();

    return changed;
  }

  // What a removal leaves behind never reaches the disk unless the compiler takes it
  private void requireCompiles(FilePatch patch) {
    if (patch.getPatched() == null) {
      return;
    }

    ValidationResult result =
        compileValidator.validate(Map.of(patch.getFile(), patch.getPatched()), Set.of());
    if (!result.isSuccess()) {
      CompileDiagnostic error = result.getErrors().get(0);
      throw new SourceModificationException(
          "The change leaves " + Path.of(patch.getFile()).getFileName() + " with an error at line "
              + error.getLine() + ", " + error.getMessage());
    }
  }

  private static List<ComponentSlice> findSlices(CompilationUnit cu,
      List<SourceLocation> components, boolean complete) {
    SourceLocation root = components.get(0);
    List<ComponentSlice> slices = new ArrayList<>();
    slices.add(ComponentSlice.of(cu, root));
    for (SourceLocation component : components.subList(1, components.size())) {
      // What another file creates is the inside of a project component and stays where it is
      if (!isSameFile(Path.of(root.getFile()), Path.of(component.getFile()))) {
        continue;
      }
      try {
        slices.add(ComponentSlice.of(cu, component));
      } catch (SourceModificationException e) {
        // A child built inside the root's own statements travels with them, a prefix icon
        if (complete && !isInside(slices.get(0), component)) {
          throw e;
        }
      }
    }

    return slices;
  }

  private static boolean isInside(ComponentSlice root, SourceLocation component) {
    Integer line = component.getLine();
    if (line == null) {
      return false;
    }
    List<Statement> statements = new ArrayList<>(root.getStatements());
    VariableDeclarator declarator = root.getDeclarator();
    if (declarator != null) {
      AstFinder.findAncestor(declarator, Statement.class).ifPresent(statements::add);
      AstFinder.findAncestor(declarator, FieldDeclaration.class)
          .ifPresent(field -> field.getRange().ifPresent(range -> {
            if (range.begin.line <= line && line <= range.end.line) {
              statements.add(new ExpressionStmt());
            }
          }));
    }
    for (Statement statement : statements) {
      boolean covers = statement.getRange()
          .map(range -> range.begin.line <= line && line <= range.end.line).orElse(false);
      if (covers) {
        return true;
      }
    }

    return false;
  }

  private static Set<String> getNames(List<SourceLocation> components,
      Map<String, String> renames) {
    SourceLocation root = components.get(0);
    Set<String> names = new LinkedHashSet<>();
    for (SourceLocation component : components) {
      String name = component.getVariableName();
      if (name != null && !name.isEmpty()
          && isSameFile(Path.of(root.getFile()), Path.of(component.getFile()))) {
        names.add(renames.getOrDefault(name, name));
      }
    }

    return names;
  }

  private static VariableDeclarator findSibling(CompilationUnit cu, ParentReference parent,
      AttachPoint to) {
    if (to.getAnchor() != null) {
      return ComponentSlice.of(cu, to.getAnchor()).getDeclarator();
    }

    Expression last = findLastChild(cu, parent, to);
    if (last == null || !(last instanceof NameExpr name)) {
      return null;
    }

    // A local of the method that attaches wins over a field or a local of the same name elsewhere
    CallableDeclaration<?> callable = findCallable(name);
    List<VariableDeclarator> candidates = cu.findAll(VariableDeclarator.class).stream()
        .filter(candidate -> candidate.getNameAsString().equals(name.getNameAsString())).toList();

    return candidates.stream().filter(candidate -> findCallable(candidate) == callable).findFirst()
        .or(() -> candidates.stream()
            .filter(candidate -> candidate.getParentNode().orElse(null) instanceof FieldDeclaration)
            .findFirst())
        .orElse(null);
  }

  // A file that creates the neighbours inline gets the new component inline as well
  private static boolean isInlineStyle(CompilationUnit cu, ParentReference parent, AttachPoint to) {
    if (to.getAnchor() != null) {
      return true;
    }

    Expression last = findLastChild(cu, parent, to);

    return last != null && !(last instanceof NameExpr);
  }

  private static Expression findLastChild(CompilationUnit cu, ParentReference parent,
      AttachPoint to) {
    Expression last = null;
    for (MethodCallExpr call : cu.findAll(MethodCallExpr.class)) {
      if (!call.getArguments().isEmpty() && to.getMethodNames().contains(call.getNameAsString())
          && parent.isCallOnParent(call)) {
        last = call.getArgument(call.getArguments().size() - 1);
      }
    }

    return last;
  }

  private static boolean isVisible(List<ComponentSlice> slices, Statement attached) {
    ComponentSlice root = slices.get(0);
    ClassOrInterfaceDeclaration into =
        AstFinder.findAncestor(attached, ClassOrInterfaceDeclaration.class).orElse(null);
    if (root.isField()) {
      return AstFinder.findAncestor(root.getDeclarator(), ClassOrInterfaceDeclaration.class)
          .orElse(null) == into;
    }

    Statement declaration = root.getDeclarationStatement();
    BlockStmt block = (BlockStmt) declaration.getParentNode().orElseThrow();
    Node current = attached;
    while (current != null && current.getParentNode().filter(holder -> holder == block).isEmpty()) {
      current = current.getParentNode().orElse(null);
    }

    return current != null
        && block.getStatements().indexOf(declaration) < block.getStatements().indexOf(current);
  }

  private static Set<String> getVisibleNames(Statement attached) {
    Set<String> names = new LinkedHashSet<>();
    CallableDeclaration<?> callable = findCallable(attached);
    if (callable != null) {
      callable.getParameters().forEach(parameter -> names.add(parameter.getNameAsString()));
    }

    Node current = attached;
    while (current.getParentNode().orElse(null) instanceof Node parent
        && !(parent instanceof CallableDeclaration)) {
      if (parent instanceof BlockStmt block) {
        int limit = block.getStatements().indexOf(current);
        for (int i = 0; i < limit; i++) {
          block.getStatement(i).findAll(VariableDeclarationExpr.class).stream()
              .filter(
                  declaration -> declaration.getParentNode().orElse(null) instanceof ExpressionStmt)
              .flatMap(declaration -> declaration.getVariables().stream())
              .forEach(variable -> names.add(variable.getNameAsString()));
        }
      }

      current = parent;
    }

    return names;
  }

  // A call that places the component inside its parent only holds once the component is attached
  private static void followAttach(List<Statement> itemCalls, Statement attached) {
    BlockStmt block = (BlockStmt) attached.getParentNode().orElseThrow();
    int index = block.getStatements().indexOf(attached);
    for (Statement itemCall : itemCalls) {
      if (itemCall.getParentNode().orElse(null) == block
          && block.getStatements().indexOf(itemCall) < index) {
        Statement copy = DetachWriter.copy(itemCall);
        itemCall.remove();
        index = block.getStatements().indexOf(attached);
        block.addStatement(index + 1, copy);
      }
    }
  }

  private static CallableDeclaration<?> findCallable(Node node) {
    return AstFinder.findAncestor(node, CallableDeclaration.class).orElse(null);
  }

  private static void addField(ClassOrInterfaceDeclaration type, FieldDeclaration field) {
    List<FieldDeclaration> fields = type.getFields();
    if (fields.isEmpty()) {
      type.getMembers().addFirst(field);
    } else {
      type.getMembers().addAfter(field, fields.get(fields.size() - 1));
    }
  }

  private static ClassOrInterfaceDeclaration findClass(CompilationUnit cu, ParentReference parent) {
    VariableDeclarator declarator = parent.getDeclarator();
    if (declarator != null) {
      return AstFinder.findAncestor(declarator, ClassOrInterfaceDeclaration.class).orElseThrow();
    }

    return cu.findFirst(ClassOrInterfaceDeclaration.class)
        .orElseThrow(() -> new SourceModificationException(
            "No class found in " + ComponentSlice.getFileName(parent.getLocation())));
  }

  private Statement parseStatement(ComponentCreation creation, String code) {
    Statement statement = parserService.parseStatement(code);
    if (statement == null) {
      throw new SourceModificationException("The creation of " + creation.getSimpleType()
          + " does not parse, " + creation.getExpression());
    }

    return statement;
  }

  // The argument keeps its own text, the printer writes a new implicit lambda parameter as " item"
  private Expression parseExpression(String method, String argument) {
    Statement statement = parserService.parseStatement("Object value = " + argument + ";");
    if (statement == null) {
      throw new SourceModificationException(
          "An argument of " + method + " does not parse, " + argument);
    }

    LexicalPreservingPrinter.setup(statement);
    VariableDeclarator variable =
        statement.asExpressionStmt().getExpression().asVariableDeclarationExpr().getVariable(0);
    Expression value = variable.getInitializer().orElseThrow();
    variable.removeInitializer();

    return value;
  }

  private Expression parseExpression(ComponentCreation creation) {
    Statement statement =
        parseStatement(creation, "Object value = " + creation.getExpression() + ";");

    return statement.asExpressionStmt().getExpression().asVariableDeclarationExpr().getVariable(0)
        .getInitializer().orElseThrow().clone();
  }

  private static void requireSameFile(SourceLocation component, SourceLocation parent) {
    if (!isSameFile(Path.of(component.getFile()), Path.of(parent.getFile()))) {
      throw new SourceModificationException(ComponentSlice.describe(component) + " is created in "
          + ComponentSlice.getFileName(component) + " and attached in "
          + ComponentSlice.getFileName(parent) + ", the two must be one file");
    }
  }

  private static void requireOutside(List<SourceLocation> components, AttachPoint to) {
    for (SourceLocation component : components) {
      if (isSame(component, to.getParent())) {
        throw new SourceModificationException("A component cannot be moved into itself");
      }
    }
  }

  private static boolean isSame(SourceLocation first, SourceLocation second) {
    return isSameFile(Path.of(first.getFile()), Path.of(second.getFile()))
        && Objects.equals(first.getVariableName(), second.getVariableName())
        && Objects.equals(first.getSimpleTypeName(), second.getSimpleTypeName())
        && (first.getVariableName() != null || Objects.equals(first.getLine(), second.getLine()));
  }

  private static boolean isSameFile(Path first, Path second) {
    return first.toAbsolutePath().normalize().equals(second.toAbsolutePath().normalize());
  }

  private static String decapitalize(String name) {
    return Character.toLowerCase(name.charAt(0)) + name.substring(1);
  }

  /**
   * The creation an insert wrote, found again in the patched text.
   */
  private static final class Created {
    private Class<? extends Node> kind;
    private String text;
    private int index = -1;
    private String name;
  }

  /**
   * What a cut hands to the paste in another file.
   */
  private static final class Transfer {
    private CompilationUnit source;
    private CutResult result;
    private Set<String> names;
  }
}
