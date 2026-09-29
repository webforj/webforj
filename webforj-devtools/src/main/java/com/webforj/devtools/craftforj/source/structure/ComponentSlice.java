package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithArguments;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.resolver.MethodResolver;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import com.webforj.devtools.craftforj.source.resolver.VariableResolver;
import com.webforj.devtools.craftforj.source.site.SourceSites;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Every piece of source that belongs to one component.
 *
 * <p>
 * A slice holds the declaration of a component, the statements that configure it and the calls that
 * pass it to something else. It is read from the Java shapes alone and knows no component by name.
 * A reference that fits none of those roles is kept as unexplained, which is what stops a removal
 * or a move from breaking the file.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class ComponentSlice {

  private final SourceLocation location;
  private final List<Statement> statements = new ArrayList<>();
  private final List<Expression> arguments = new ArrayList<>();
  private final List<Node> unexplained = new ArrayList<>();
  private VariableDeclarator declarator;
  private Expression inline;

  private ComponentSlice(SourceLocation location) {
    this.location = location;
  }

  /**
   * Reads the slice of the component at the given location.
   *
   * @param cu the parsed file
   * @param location the component location
   *
   * @return the slice
   *
   * @throws SourceModificationException when the component has no creation in the file
   */
  static ComponentSlice of(CompilationUnit cu, SourceLocation location) {
    ComponentSlice slice = new ComponentSlice(location);
    if (location.getSite() != null) {
      slice.read(SourceSites.find(cu, location.getSite()));

      return slice;
    }

    String name = location.getVariableName();
    if (name != null && !name.isEmpty()) {
      slice.declarator = findDeclarator(cu, location);
      slice.collectReferences();

      return slice;
    }

    slice.readInline(findInlineCreation(cu, location));

    return slice;
  }

  /**
   * Finds the declaration of a variable holding a component.
   *
   * @param cu the parsed file
   * @param location the component location
   *
   * @return the declarator
   *
   * @throws SourceModificationException when none or more than one declaration fits
   */
  static VariableDeclarator findDeclarator(CompilationUnit cu, SourceLocation location) {
    if (location.getSite() != null) {
      VariableDeclarator declared = findDeclared(SourceSites.find(cu, location.getSite()));
      if (declared != null) {
        return declared;
      }
    }

    String name = location.getVariableName();
    String type = location.getSimpleTypeName();
    List<VariableDeclarator> matches = new ArrayList<>();
    for (VariableDeclarator candidate : cu.findAll(VariableDeclarator.class)) {
      if (candidate.getNameAsString().equals(name) && isDeclaredBy(candidate, location)
          && (hasCreationOf(cu, candidate, type) || AstFinder.matchesType(candidate.getType(),
              candidate.getInitializer().orElse(null), type))) {
        matches.add(candidate);
      }
    }

    if (matches.size() > 1 && location.getLine() != null) {
      int line = location.getLine();
      List<VariableDeclarator> anchored = matches.stream()
          .filter(candidate -> candidate.getRange()
              .map(range -> range.begin.line <= line && range.end.line >= line).orElse(false))
          .toList();
      if (anchored.size() == 1) {
        return anchored.get(0);
      }
    }

    if (matches.size() != 1) {
      throw new SourceModificationException(matches.isEmpty()
          ? "No declaration of " + describe(location) + " found in " + getFileName(location)
          : "More than one declaration of " + describe(location) + " found in "
              + getFileName(location));
    }

    return matches.get(0);
  }

  /**
   * Gets the location the slice was read from.
   *
   * @return the component location
   */
  SourceLocation getLocation() {
    return location;
  }

  /**
   * Gets the variable name.
   *
   * @return the variable name, or {@code null} for a component created inline
   */
  String getVariableName() {
    return declarator == null ? null : declarator.getNameAsString();
  }

  /**
   * Gets the declarator.
   *
   * @return the declarator, or {@code null} for a component created inline
   */
  VariableDeclarator getDeclarator() {
    return declarator;
  }

  /**
   * Gets the expression of a component created inline.
   *
   * @return the whole inline expression, or {@code null} for a declared component
   */
  Expression getInlineCreation() {
    return inline;
  }

  /**
   * Checks whether the component is declared as a field.
   *
   * @return {@code true} for a field
   */
  boolean isField() {
    return declarator != null && VariableResolver.isField(declarator);
  }

  /**
   * Gets the statement declaring a local component.
   *
   * @return the declaration statement, or {@code null} for a field or an inline component
   */
  Statement getDeclarationStatement() {
    if (declarator == null || isField()) {
      return null;
    }

    return AstFinder.findAncestor(declarator, Statement.class).orElse(null);
  }

  /**
   * Gets the statements that configure the component, in source order.
   *
   * @return the statements, an assignment creating a field included
   */
  List<Statement> getStatements() {
    return statements;
  }

  /**
   * Gets the expressions that pass the component to a call or a creation.
   *
   * @return the argument expressions
   */
  List<Expression> getArguments() {
    return arguments;
  }

  /**
   * Gets the references that fit no known role.
   *
   * @return the unexplained nodes
   */
  List<Node> getUnexplained() {
    return unexplained;
  }

  /**
   * Gets the call or the creation an argument expression is passed to.
   *
   * @param argument an argument of this slice
   *
   * @return the receiving node
   */
  static Node getReceiver(Expression argument) {
    return argument.getParentNode().orElseThrow();
  }

  /**
   * Describes a location for a message.
   *
   * @param location the component location
   *
   * @return the variable and type, or the type and line for an inline component
   */
  static String describe(SourceLocation location) {
    String name = location.getVariableName();
    if (name != null && !name.isEmpty()) {
      return location.getSimpleTypeName() + " " + name;
    }

    return location.getSimpleTypeName() + " at line " + location.getLine();
  }

  /**
   * Gets the file name of a location for a message.
   *
   * @param location the component location
   *
   * @return the file name without its folders
   */
  static String getFileName(SourceLocation location) {
    return Path.of(location.getFile()).getFileName().toString();
  }

  /**
   * Checks whether a statement is one of the statements a block or a case group lists.
   *
   * @param statement the statement
   *
   * @return {@code true} for a statement of a block or of a case group
   */
  static boolean isListed(Statement statement) {
    Node holder = statement.getParentNode().orElse(null);

    return holder instanceof BlockStmt || holder instanceof SwitchEntry;
  }

  /**
   * Finds the statement that holds a node and can be taken out of the code around it.
   *
   * @param node the node
   *
   * @return the nearest statement that can be taken out
   *
   * @throws SourceModificationException when no such statement holds the node
   */
  static Statement findStatement(Node node) {
    Node current = node;
    do {
      if (current instanceof Statement statement && isRemovable(statement)) {
        return statement;
      }

      current = current.getParentNode().orElse(null);
    } while (current != null);

    throw new SourceModificationException("The code at line "
        + node.getRange().map(range -> range.begin.line).orElse(0) + " is not inside a block");
  }

  /**
   * Checks whether a statement can be taken out of the code that holds it.
   *
   * @param statement the statement
   *
   * @return {@code true} for a statement of a block or of a case group, and for one that is the
   *         whole body of a branch, of a loop or of a lambda
   */
  private static boolean isRemovable(Statement statement) {
    Node holder = statement.getParentNode().orElse(null);

    return isListed(statement) || !(statement instanceof BlockStmt)
        && (holder instanceof Statement || holder instanceof LambdaExpr);
  }

  private static VariableDeclarator findDeclared(Expression creation) {
    return VariableResolver.findHolder(MethodResolver.findChainEnd(creation));
  }

  // A class the compiler numbered, an anonymous or a local one, has no name the source could be
  // asked for
  private static boolean isDeclaredBy(VariableDeclarator candidate, SourceLocation location) {
    String owner = location.getDeclaringClass();
    if (owner == null || owner.isBlank() || owner.matches(".*\\$\\d.*")) {
      return true;
    }

    return VariableResolver.findEnclosingType(candidate) instanceof TypeDeclaration<?> type
        && TypeResolver.getBinaryName(type).equals(owner);
  }

  private static boolean hasCreationOf(CompilationUnit cu, VariableDeclarator candidate,
      String type) {
    if (type == null || !VariableResolver.isField(candidate)) {
      return false;
    }

    return cu.findAll(AssignExpr.class).stream()
        .anyMatch(assign -> VariableResolver.findDeclaration(assign.getTarget()) == candidate
            && assign.getValue() instanceof ObjectCreationExpr creation
            && creation.getType().getNameAsString().equals(type));
  }

  private static Expression findInlineCreation(CompilationUnit cu, SourceLocation location) {
    if (location.getLine() == null) {
      throw new SourceModificationException(
          "No creation of " + describe(location) + " found in " + getFileName(location));
    }

    TargetContext target = new TargetContext(location.getLine(), location.getSimpleTypeName());
    long sameLine = cu.findAll(ObjectCreationExpr.class).stream()
        .filter(candidate -> candidate.getType().getNameAsString().equals(target.getTypeName())
            && candidate.getRange().map(range -> range.begin.line == target.getLineNumber())
                .orElse(false))
        .count();
    if (sameLine > 1) {
      throw new SourceModificationException("More than one " + location.getSimpleTypeName()
          + " is created at line " + location.getLine() + ", the line alone cannot tell which");
    }

    Expression creation = AstFinder.findInlineCreationAt(cu, target).map(Expression.class::cast)
        .or(() -> AstFinder.findFactoryMethodAt(cu, target)).orElse(null);
    if (creation == null) {
      boolean routed = cu.findFirst(ClassOrInterfaceDeclaration.class)
          .filter(type -> type.getNameAsString().equals(location.getSimpleTypeName())
              && type.getAnnotationByName("Route").isPresent())
          .isPresent();

      throw new SourceModificationException(routed
          ? location.getSimpleTypeName() + " is placed by the router, its @Route decides where it "
              + "renders"
          : "No creation of " + describe(location) + " found in " + getFileName(location));
    }

    return creation;
  }

  private void read(Expression creation) {
    declarator = findDeclared(creation);
    if (declarator != null) {
      collectReferences();
    } else {
      readInline(creation);
    }
  }

  // A fluent chain on the creation travels with it, new Button("Save").setTheme(...)
  private void readInline(Expression creation) {
    inline = MethodResolver.findChainEnd(creation);
    Node holder = inline.getParentNode().orElse(null);
    if (holder instanceof NodeWithArguments<?> receiver
        && receiver.getArguments().stream().anyMatch(argument -> argument == inline)) {
      arguments.add(inline);

      return;
    }

    if (findStatementOf(inline) != null) {
      statements.add(findStatementOf(inline));

      return;
    }

    throw new SourceModificationException("The creation of " + describe(location)
        + " is neither passed to a call nor a statement of its own");
  }

  private void collectReferences() {
    List<Expression> nested = new ArrayList<>();
    Node owner = VariableResolver.findEnclosingType(declarator);
    for (Expression reference : VariableResolver.findReferences(declarator)) {
      if (findHost(reference) != null) {
        nested.add(reference);
      } else if (VariableResolver.findEnclosingType(reference) != owner) {
        // A use written in another class is that class's business and stays with it
        unexplained.add(reference);
      } else {
        classify(reference);
      }
    }

    // A reference inside a lambda is fine when the statement holding it goes with the component
    for (Expression reference : nested) {
      Statement holder = AstFinder.findAncestor(findHost(reference), Statement.class).orElse(null);
      if (holder == null || !statements.contains(holder)) {
        unexplained.add(reference);
      }
    }
  }

  private void classify(Expression reference) {
    Node parent = reference.getParentNode().orElse(null);

    if (parent instanceof AssignExpr assign && assign.getTarget() == reference
        && assign.getParentNode().orElse(null) instanceof ExpressionStmt statement
        && isRemovable(statement)) {
      addStatement(statement);
      return;
    }

    Expression value = MethodResolver.findChainEnd(reference);
    if (value.getParentNode().orElse(null) instanceof NodeWithArguments<?> receiver
        && receiver.getArguments().stream().anyMatch(argument -> argument == value)) {
      arguments.add(value);
      return;
    }

    Statement statement = findStatementOf(reference);
    if (statement != null) {
      addStatement(statement);
      return;
    }

    unexplained.add(reference);
  }

  // A statement that only calls something on the expression exists for the component alone
  private static Statement findStatementOf(Expression expression) {
    Expression chain = expression;
    while (chain.getParentNode().orElse(null) instanceof MethodCallExpr call
        && call.getScope().orElse(null) == chain) {
      chain = call;
    }

    boolean called = chain != expression || expression instanceof MethodCallExpr
        || expression instanceof ObjectCreationExpr;

    return called && chain.getParentNode().orElse(null) instanceof ExpressionStmt statement
        && isRemovable(statement) ? statement : null;
  }

  private void addStatement(Statement statement) {
    if (!statements.contains(statement)) {
      statements.add(statement);
    }
  }

  // A lambda or an anonymous class the component itself is declared in hides nothing from it
  private Node findHost(Expression reference) {
    Node host = null;
    Node current = reference.getParentNode().orElse(null);
    while (current != null && !current.isAncestorOf(declarator)) {
      if (current instanceof LambdaExpr || (current instanceof ObjectCreationExpr creation
          && creation.getAnonymousClassBody().isPresent())) {
        host = current;
      }

      current = current.getParentNode().orElse(null);
    }

    return host;
  }
}
