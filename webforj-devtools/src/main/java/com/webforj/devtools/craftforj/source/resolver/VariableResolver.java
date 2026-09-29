package com.webforj.devtools.craftforj.source.resolver;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.EnumDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.InitializerDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.RecordDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodReferenceExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.TypeExpr;
import com.github.javaparser.ast.expr.TypePatternExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.nodeTypes.SwitchNode;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Binds the names written in a source file to the variables they stand for.
 *
 * <p>
 * A name binds to the nearest declaration the Java scoping rules make visible at its place, a
 * local, a parameter, a resource, a loop variable or a field. The same name in another scope binds
 * to another variable and is never mistaken for it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class VariableResolver {

  private VariableResolver() {}

  /**
   * Finds the declaration a name is bound to by the declarations of its own file.
   *
   * @param reference a name, a {@code this.name} field access or either one in parentheses
   *
   * @return the variable declarator or the parameter, or {@code null} when the file declares
   *         nothing visible under that name
   */
  public static Node findDeclaration(Expression reference) {
    return find(reference, false);
  }

  /**
   * Finds every expression that reads or writes the given variable.
   *
   * @param declaration the variable
   *
   * @return the names, the field accesses and the method reference scopes bound to the variable, in
   *         source order
   *
   * @throws SourceModificationException when a use of the same name cannot be told apart from the
   *         variable
   */
  public static List<Expression> findReferences(VariableDeclarator declaration) {
    String name = declaration.getNameAsString();
    Node root = declaration.findCompilationUnit().map(Node.class::cast)
        .orElseGet(() -> findRoot(declaration));
    List<Expression> references = new ArrayList<>();
    root.walk(Expression.class, expression -> {
      if (isNamed(expression, name) && isReachable(expression, declaration)
          && find(expression, true) == declaration) {
        references.add(expression);
      }
    });
    references.sort(Comparator.comparing(VariableResolver::getLine)
        .thenComparing(node -> node.getRange().map(range -> range.begin.column).orElse(0)));

    return references;
  }

  /**
   * Finds the variable that holds the value of an expression.
   *
   * @param value the expression
   *
   * @return the variable the expression initializes or is assigned to, or {@code null} when the
   *         value goes somewhere else
   */
  public static VariableDeclarator findHolder(Expression value) {
    Node holder = value.getParentNode().orElse(null);
    if (holder instanceof VariableDeclarator variable) {
      return variable;
    }

    boolean assigned = holder instanceof AssignExpr assign && assign.getValue() == value;

    return assigned
        && findDeclaration(((AssignExpr) holder).getTarget()) instanceof VariableDeclarator variable
            ? variable
            : null;
  }

  /**
   * Checks whether a variable is declared as a field.
   *
   * @param declaration the variable
   *
   * @return {@code true} for a field, {@code false} for a local
   */
  public static boolean isField(VariableDeclarator declaration) {
    return declaration.getParentNode().orElse(null) instanceof FieldDeclaration;
  }

  /**
   * Finds the class body a node is written in.
   *
   * @param node the node
   *
   * @return the type declaration or the creation of the anonymous class, or {@code null} when the
   *         node is outside every class
   */
  public static Node findEnclosingType(Node node) {
    Node current = node.getParentNode().orElse(null);
    while (current != null && !isClassScope(current)) {
      current = current.getParentNode().orElse(null);
    }

    return current;
  }

  // A name outside the reach of the variable binds to something else, whatever that is
  private static boolean isReachable(Expression expression, VariableDeclarator declaration) {
    if (expression instanceof FieldAccessExpr) {
      return isField(declaration);
    }

    if (isField(declaration)) {
      Node type = findEnclosingType(declaration);

      return type != null && type.isAncestorOf(expression);
    }

    Node scope = declaration.getParentNode().flatMap(Node::getParentNode).orElse(null);
    while (scope != null && !(scope instanceof BlockStmt) && !(scope instanceof SwitchNode)
        && !(scope instanceof ForStmt) && !(scope instanceof ForEachStmt)
        && !(scope instanceof TryStmt)) {
      scope = scope.getParentNode().orElse(null);
    }

    return scope != null && scope.isAncestorOf(expression) && declaration.getBegin()
        .flatMap(start -> expression.getBegin().map(start::isBefore)).orElse(false);
  }

  private static Node find(Expression reference, boolean strict) {
    if (reference instanceof EnclosedExpr enclosed) {
      return find(enclosed.getInner(), strict);
    }

    if (reference instanceof NameExpr name) {
      return findByName(name, name.getNameAsString(), strict);
    }

    if (reference instanceof TypeExpr type) {
      return findByName(type, type.getType().asString(), strict);
    }

    if (reference instanceof FieldAccessExpr access) {
      return findField(access, strict);
    }

    return null;
  }

  private static boolean isNamed(Expression expression, String name) {
    if (expression instanceof NameExpr reference) {
      return reference.getNameAsString().equals(name);
    }

    if (expression instanceof FieldAccessExpr access) {
      return access.getNameAsString().equals(name);
    }

    // The parser reads the variable of target::focus as a type
    return expression instanceof TypeExpr type
        && type.getParentNode().orElse(null) instanceof MethodReferenceExpr
        && type.getType().asString().equals(name);
  }

  private static Node findByName(Node reference, String name, boolean strict) {
    if (strict) {
      requireNoPattern(reference, name);
    }

    Node child = reference;
    Node scope = child.getParentNode().orElse(null);
    while (scope != null) {
      Node declaration = findVisible(scope, child, name);
      if (declaration != null) {
        return declaration;
      }

      if (strict && isClassBody(scope, child) && inheritsField(scope, name, reference)) {
        return null;
      }

      child = scope;
      scope = scope.getParentNode().orElse(null);
    }

    return null;
  }

  private static Node findField(FieldAccessExpr access, boolean strict) {
    String name = access.getNameAsString();
    Expression scope = access.getScope();
    if (scope instanceof ThisExpr self) {
      String qualifier = self.getTypeName().map(Node::toString).orElse(null);
      Node type = findEnclosingType(access);
      while (type != null && qualifier != null && !(type instanceof TypeDeclaration<?> declared
          && declared.getNameAsString().equals(qualifier))) {
        type = findEnclosingType(type);
      }

      return type == null ? null : findDeclaredField(type, name);
    }

    return !strict || scope instanceof SuperExpr ? null : findFieldOfInstance(access);
  }

  // A field reached through another instance is the same variable when that instance is of the
  // class declaring it
  private static Node findFieldOfInstance(FieldAccessExpr access) {
    String name = access.getNameAsString();
    Expression scope = access.getScope();
    TypeDeclaration<?> written = findWrittenType(scope);
    if (written != null) {
      return findDeclaredField(written, name);
    }

    Class<?> owner = TypeResolver.resolveType(scope);
    if (owner == null && !isTypeOrPackageName(scope)) {
      throw new SourceModificationException(name + " is reached through " + scope + " at line "
          + getLine(access) + ", what that stands for cannot be resolved");
    }

    CompilationUnit cu = access.findCompilationUnit().orElse(null);
    if (owner == null || cu == null) {
      return null;
    }

    return cu.findAll(TypeDeclaration.class).stream()
        .filter(type -> TypeResolver.load(TypeResolver.getBinaryName(type)) == owner).findFirst()
        .map(type -> findDeclaredField(type, name)).orElse(null);
  }

  private static TypeDeclaration<?> findWrittenType(Expression scope) {
    Node declaration = find(scope, false);
    if (declaration instanceof VariableDeclarator variable
        && variable.getType() instanceof ClassOrInterfaceType type) {
      return TypeResolver.findDeclaredType(type, variable);
    }

    return declaration instanceof Parameter parameter
        && parameter.getType() instanceof ClassOrInterfaceType type
            ? TypeResolver.findDeclaredType(type, parameter)
            : null;
  }

  private static boolean isTypeOrPackageName(Expression scope) {
    Expression current = scope;
    while (current instanceof FieldAccessExpr access) {
      current = access.getScope();
    }

    return current instanceof NameExpr name
        && findByName(name, name.getNameAsString(), false) == null;
  }

  private static Node findVisible(Node scope, Node child, String name) {
    Node local = findLocal(scope, child, name);
    if (local != null) {
      return local;
    }

    if (scope instanceof CallableDeclaration<?> callable) {
      return findParameter(callable.getParameters(), name);
    }

    if (scope instanceof LambdaExpr function) {
      return findParameter(function.getParameters(), name);
    }

    if (scope instanceof CatchClause clause) {
      return findParameter(List.of(clause.getParameter()), name);
    }

    if (scope instanceof RecordDeclaration data) {
      Node component = findParameter(data.getParameters(), name);
      if (component != null) {
        return component;
      }
    }

    return isClassBody(scope, child) ? findDeclaredField(scope, name) : null;
  }

  private static Node findLocal(Node scope, Node child, String name) {
    if (scope instanceof BlockStmt block) {
      return findInStatements(block.getStatements(), child, name);
    }

    if (scope instanceof SwitchEntry entry) {
      return findInStatements(entry.getStatements(), child, name);
    }

    if (scope instanceof SwitchNode branch) {
      return findInEarlierEntries(branch, child, name);
    }

    if (scope instanceof VariableDeclarationExpr variables) {
      return findVariable(variables, name, AstFinder.indexOfSame(variables.getVariables(), child));
    }

    if (scope instanceof ForEachStmt loop && child == loop.getBody()) {
      return findVariable(loop.getVariable(), name, loop.getVariable().getVariables().size());
    }

    if (scope instanceof ForStmt loop) {
      return findInExpressions(loop.getInitialization(), child, name);
    }

    boolean sees = scope instanceof TryStmt attempt && (child == attempt.getTryBlock()
        || AstFinder.indexOfSame(attempt.getResources(), child) >= 0);

    return sees ? findInExpressions(((TryStmt) scope).getResources(), child, name) : null;
  }

  // The arguments of an anonymous class creation are written outside of its body
  private static boolean isClassBody(Node scope, Node child) {
    return scope instanceof TypeDeclaration<?> || scope instanceof ObjectCreationExpr creation
        && creation.getAnonymousClassBody().isPresent() && child instanceof BodyDeclaration<?>;
  }

  private static Node findInStatements(List<Statement> statements, Node child, String name) {
    int before = AstFinder.indexOfSame(statements, child);
    int limit = before < 0 ? statements.size() : before;
    for (int index = limit - 1; index >= 0; index--) {
      if (statements.get(index) instanceof ExpressionStmt statement
          && statement.getExpression() instanceof VariableDeclarationExpr variables) {
        Node match = findVariable(variables, name, variables.getVariables().size());
        if (match != null) {
          return match;
        }
      }
    }

    return null;
  }

  // A local of a case group stays visible in the groups written after it
  private static Node findInEarlierEntries(SwitchNode branch, Node child, String name) {
    if (!(child instanceof SwitchEntry current)
        || current.getType() != SwitchEntry.Type.STATEMENT_GROUP) {
      return null;
    }

    int before = AstFinder.indexOfSame(branch.getEntries(), child);
    for (int index = before - 1; index >= 0; index--) {
      SwitchEntry entry = branch.getEntries().get(index);
      if (entry.getType() != SwitchEntry.Type.STATEMENT_GROUP) {
        continue;
      }

      Node match = findInStatements(entry.getStatements(), null, name);
      if (match != null) {
        return match;
      }
    }

    return null;
  }

  private static Node findInExpressions(List<Expression> expressions, Node child, String name) {
    int before = AstFinder.indexOfSame(expressions, child);
    int limit = before < 0 ? expressions.size() : before;
    for (int index = limit - 1; index >= 0; index--) {
      if (expressions.get(index) instanceof VariableDeclarationExpr variables) {
        Node match = findVariable(variables, name, variables.getVariables().size());
        if (match != null) {
          return match;
        }
      }
    }

    return null;
  }

  private static Node findParameter(List<Parameter> parameters, String name) {
    return parameters.stream().filter(parameter -> parameter.getNameAsString().equals(name))
        .findFirst().orElse(null);
  }

  private static VariableDeclarator findVariable(VariableDeclarationExpr variables, String name,
      int limit) {
    int last = limit < 0 ? variables.getVariables().size() : limit;
    for (int index = last - 1; index >= 0; index--) {
      VariableDeclarator variable = variables.getVariable(index);
      if (variable.getNameAsString().equals(name)) {
        return variable;
      }
    }

    return null;
  }

  private static VariableDeclarator findDeclaredField(Node type, String name) {
    return getMembers(type).stream().filter(FieldDeclaration.class::isInstance)
        .map(FieldDeclaration.class::cast).flatMap(field -> field.getVariables().stream())
        .filter(variable -> variable.getNameAsString().equals(name)).findFirst().orElse(null);
  }

  private static List<BodyDeclaration<?>> getMembers(Node type) {
    if (type instanceof TypeDeclaration<?> declared) {
      return new ArrayList<>(declared.getMembers());
    }

    List<BodyDeclaration<?>> members = new ArrayList<>();
    if (type instanceof ObjectCreationExpr creation) {
      creation.getAnonymousClassBody().ifPresent(members::addAll);
    }

    return members;
  }

  private static boolean inheritsField(Node type, String name, Node reference) {
    for (ClassOrInterfaceType parent : getParents(type)) {
      if (hasField(parent, type, name, reference)) {
        return true;
      }
    }

    return false;
  }

  private static List<ClassOrInterfaceType> getParents(Node type) {
    List<ClassOrInterfaceType> parents = new ArrayList<>();
    if (type instanceof ClassOrInterfaceDeclaration declared) {
      parents.addAll(declared.getExtendedTypes());
      parents.addAll(declared.getImplementedTypes());
    } else if (type instanceof EnumDeclaration declared) {
      parents.addAll(declared.getImplementedTypes());
    } else if (type instanceof RecordDeclaration declared) {
      parents.addAll(declared.getImplementedTypes());
    } else if (type instanceof ObjectCreationExpr creation) {
      parents.add(creation.getType());
    }

    return parents;
  }

  private static boolean hasField(ClassOrInterfaceType parent, Node at, String name,
      Node reference) {
    TypeDeclaration<?> written = TypeResolver.findDeclaredType(parent, at);
    if (written != null) {
      boolean declares = written.getFields().stream().anyMatch(field -> !field.isPrivate() && field
          .getVariables().stream().anyMatch(variable -> variable.getNameAsString().equals(name)));

      return declares || inheritsField(written, name, reference);
    }

    Class<?> loaded = TypeResolver.resolve(parent, at);
    if (loaded == null) {
      throw new SourceModificationException(name + " at line " + getLine(reference)
          + " may be a field of " + parent.getNameAsString() + ", which cannot be resolved");
    }

    try {
      return hasField(loaded, name);
    } catch (LinkageError | SecurityException e) {
      throw new SourceModificationException(name + " at line " + getLine(reference)
          + " may be a field of " + parent.getNameAsString() + ", which cannot be read");
    }
  }

  private static boolean hasField(Class<?> type, String name) {
    for (Field field : type.getFields()) {
      if (field.getName().equals(name)) {
        return true;
      }
    }

    for (Class<?> current = type; current != null; current = current.getSuperclass()) {
      for (Field field : current.getDeclaredFields()) {
        if (field.getName().equals(name) && !Modifier.isPrivate(field.getModifiers())) {
          return true;
        }
      }
    }

    return false;
  }

  // A pattern variable is visible wherever the match is known to hold, which no scope spells out
  private static void requireNoPattern(Node reference, String name) {
    Node body = findRoot(reference);
    body.findFirst(TypePatternExpr.class, pattern -> pattern.getNameAsString().equals(name))
        .ifPresent(pattern -> {
          throw new SourceModificationException(
              name + " is also matched as a pattern at line " + getLine(pattern));
        });
  }

  private static Node findRoot(Node node) {
    Node current = node;
    Node parent = current.getParentNode().orElse(null);
    while (parent != null) {
      if (parent instanceof CallableDeclaration || parent instanceof InitializerDeclaration
          || parent instanceof FieldDeclaration) {
        return parent;
      }

      current = parent;
      parent = current.getParentNode().orElse(null);
    }

    return current;
  }

  private static boolean isClassScope(Node node) {
    return node instanceof TypeDeclaration<?> || node instanceof ObjectCreationExpr creation
        && creation.getAnonymousClassBody().isPresent();
  }

  private static int getLine(Node node) {
    return node.getRange().map(range -> range.begin.line).orElse(0);
  }
}
