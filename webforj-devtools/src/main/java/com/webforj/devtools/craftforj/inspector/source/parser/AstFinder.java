package com.webforj.devtools.craftforj.inspector.source.parser;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.BinaryExpr;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.Type;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.ToIntFunction;

/**
 * Utility for finding nodes in JavaParser AST.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class AstFinder {

  private AstFinder() {}

  /**
   * Finds a field declaration at the target line.
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the field declaration if found
   */
  public static Optional<FieldDeclaration> findFieldAt(CompilationUnit cu, TargetContext target) {
    int lineNumber = target.getLineNumber();
    for (FieldDeclaration field : cu.findAll(FieldDeclaration.class)) {
      if (field.getRange().isPresent() && field.getRange().get().begin.line <= lineNumber
          && field.getRange().get().end.line >= lineNumber
          && matchesFieldType(field, acceptedTypes(target))) {
        return Optional.of(field);
      }
    }

    return Optional.empty();
  }

  /**
   * Checks whether a declaration is compatible with the expected component type.
   *
   * <p>
   * A declaration matches when its declared type or its {@code new X(...)} initializer carries the
   * expected simple type name. This guards source edits against stale line numbers: when the
   * running app's recorded line no longer points at the component's declaration (the file shifted
   * since the JVM compiled it), a type mismatch rejects the edit instead of silently rewriting a
   * neighboring component.
   * </p>
   *
   * @param declaredType the declared type, or null when unknown
   * @param initializer the initializer expression, or null
   * @param expectedTypeName the expected simple type name, or null/empty to skip the check
   *
   * @return true if the declaration is compatible with the expected type
   */
  public static boolean matchesType(Type declaredType, Expression initializer,
      String expectedTypeName) {
    return matchesType(declaredType, initializer,
        expectedTypeName == null || expectedTypeName.isEmpty() ? null : Set.of(expectedTypeName));
  }

  /**
   * Checks whether a declaration is compatible with any of the expected component types.
   *
   * @param declaredType the declared type, or null when unknown
   * @param initializer the initializer expression, or null
   * @param expectedTypeNames the acceptable simple type names, or null/empty to skip the check
   *
   * @return true if the declaration is compatible with one of the expected types
   */
  public static boolean matchesType(Type declaredType, Expression initializer,
      Collection<String> expectedTypeNames) {
    if (expectedTypeNames == null || expectedTypeNames.isEmpty()) {
      return true;
    }

    if (initializer instanceof ObjectCreationExpr creation
        && expectedTypeNames.contains(creation.getType().getNameAsString())) {
      return true;
    }

    if (declaredType == null) {
      return false;
    }

    return expectedTypeNames.contains(simpleTypeName(declaredType.asString()));
  }

  private static boolean matchesFieldType(FieldDeclaration field,
      Collection<String> expectedTypeNames) {
    Expression initializer =
        field.getVariables().isEmpty() ? null : field.getVariable(0).getInitializer().orElse(null);

    return matchesType(field.getCommonType(), initializer, expectedTypeNames);
  }

  private static Collection<String> acceptedTypes(TargetContext target) {
    if (target.getAcceptableTypes() != null && !target.getAcceptableTypes().isEmpty()) {
      return target.getAcceptableTypes();
    }

    String typeName = target.getTypeName();

    return typeName == null || typeName.isEmpty() ? null : Set.of(typeName);
  }

  private static String simpleTypeName(String typeName) {
    String name = typeName;
    int generic = name.indexOf('<');
    if (generic > 0) {
      name = name.substring(0, generic);
    }

    return name.substring(name.lastIndexOf('.') + 1).trim();
  }

  /**
   * Finds the most specific (deepest) statement at the target line.
   *
   * <p>
   * This method recursively searches into nested blocks (lambda bodies, anonymous classes, etc.) to
   * find the most specific statement that contains the given line. Uses a single-pass traversal for
   * efficiency.
   * </p>
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the most specific statement if found
   */
  public static Optional<Node> findNodeAt(CompilationUnit cu, TargetContext target) {
    int lineNumber = target.getLineNumber();
    ToIntFunction<Node> getDepth = node -> {
      int d = 0;
      Node current = node;
      while (current.getParentNode().isPresent()) {
        current = current.getParentNode().get();
        d++;
      }
      return d;
    };

    Statement[] best = {null};
    int[] bestSpan = {Integer.MAX_VALUE};
    int[] bestDepth = {-1};

    cu.walk(Statement.class, stmt -> {
      if (stmt.getRange().isEmpty()) {
        return;
      }
      var range = stmt.getRange().get();
      if (range.begin.line <= lineNumber && range.end.line >= lineNumber) {
        int span = range.end.line - range.begin.line;
        int depth = getDepth.applyAsInt(stmt);

        // Prefer smaller span, or same span with deeper nesting
        if (span < bestSpan[0] || (span == bestSpan[0] && depth > bestDepth[0])) {
          best[0] = stmt;
          bestSpan[0] = span;
          bestDepth[0] = depth;
        }
      }
    });

    return Optional.ofNullable(best[0]);
  }

  /**
   * Finds an inline object creation (new X()) at the target line for the specified type.
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the object creation expression if found
   */
  public static Optional<ObjectCreationExpr> findInlineCreationAt(CompilationUnit cu,
      TargetContext target) {
    int lineNumber = target.getLineNumber();
    String typeName = target.getTypeName();
    for (ObjectCreationExpr creation : cu.findAll(ObjectCreationExpr.class)) {
      if (creation.getRange().isPresent()) {
        int creationLine = creation.getRange().get().begin.line;
        if (creationLine == lineNumber && creation.getType().getNameAsString().equals(typeName)) {
          if (isInlineCreation(creation)) {
            return Optional.of(creation);
          }
        }
      }
    }
    return Optional.empty();
  }

  /**
   * Finds a static factory method call at the target line.
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the method call expression if found
   */
  public static Optional<MethodCallExpr> findFactoryMethodAt(CompilationUnit cu,
      TargetContext target) {
    int lineNumber = target.getLineNumber();
    for (MethodCallExpr methodCall : cu.findAll(MethodCallExpr.class)) {
      if (methodCall.getRange().isPresent()) {
        int callLine = methodCall.getRange().get().begin.line;
        if (callLine == lineNumber) {
          @SuppressWarnings("unchecked")
          Optional<VariableDeclarator> varDecl = methodCall.findAncestor(VariableDeclarator.class);
          if (varDecl.isEmpty() && methodCall.getScope().isPresent()) {
            var scope = methodCall.getScope().get();
            if (scope instanceof NameExpr nameExpr) {
              String name = nameExpr.getNameAsString();
              if (Character.isUpperCase(name.charAt(0))) {
                return Optional.of(methodCall);
              }
            } else if (scope instanceof FieldAccessExpr) {
              Expression root = scope;
              while (root instanceof FieldAccessExpr fieldAccess) {
                root = fieldAccess.getScope();
              }
              if (root instanceof NameExpr rootName
                  && Character.isUpperCase(rootName.getNameAsString().charAt(0))) {
                return Optional.of(methodCall);
              }
            }
          }
        }
      }
    }
    return Optional.empty();
  }

  /**
   * Finds icon-producing expressions at the target line.
   *
   * <p>
   * Candidates are icon factory calls ({@code TablerIcon.create("home")},
   * {@code FeatherIcon.BELL.create()}) and {@code Icon}/{@code IconButton} creations. When a
   * creation wraps a factory call ({@code new IconButton(TablerIcon.create("x"))}), only the inner
   * factory call is returned since it defines the icon name and pool.
   * </p>
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the icon expressions starting at the target line
   */
  public static List<Expression> findIconExpressionsAt(CompilationUnit cu, TargetContext target) {
    int lineNumber = target.getLineNumber();
    List<Expression> candidates = new ArrayList<>();

    for (MethodCallExpr call : cu.findAll(MethodCallExpr.class)) {
      if (call.getRange().isEmpty() || call.getRange().get().begin.line != lineNumber
          || !"create".equals(call.getNameAsString()) || call.getScope().isEmpty()) {
        continue;
      }

      Expression scope = call.getScope().get();
      boolean staticFactory = scope instanceof NameExpr nameExpr
          && Character.isUpperCase(nameExpr.getNameAsString().charAt(0));
      boolean enumFactory = scope instanceof FieldAccessExpr fieldAccess
          && fieldAccess.getScope() instanceof NameExpr enumName
          && Character.isUpperCase(enumName.getNameAsString().charAt(0));

      if (staticFactory || enumFactory) {
        candidates.add(call);
      }
    }

    for (ObjectCreationExpr creation : cu.findAll(ObjectCreationExpr.class)) {
      if (creation.getRange().isEmpty() || creation.getRange().get().begin.line != lineNumber) {
        continue;
      }

      String type = creation.getType().getNameAsString();
      if ("Icon".equals(type) || "IconButton".equals(type) || type.equals(target.getTypeName())) {
        candidates.add(creation);
      }
    }

    return candidates.stream()
        .filter(c -> candidates.stream().noneMatch(o -> o != c && c.isAncestorOf(o))).toList();
  }

  /**
   * Extracts variable name from a field declaration.
   *
   * @param field the field declaration
   *
   * @return the variable name or null if not found
   */
  public static String extractVariableName(FieldDeclaration field) {
    if (!field.getVariables().isEmpty()) {
      return field.getVariable(0).getNameAsString();
    }
    return null;
  }

  /**
   * Extracts variable name from a node at the target line.
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the variable name or null if not found
   */
  public static String extractVariableNameAt(CompilationUnit cu, TargetContext target) {
    int lineNumber = target.getLineNumber();
    Collection<String> typeNames = acceptedTypes(target);

    for (VariableDeclarationExpr varDecl : cu.findAll(VariableDeclarationExpr.class)) {
      if (varDecl.getRange().isPresent() && varDecl.getRange().get().begin.line <= lineNumber
          && varDecl.getRange().get().end.line >= lineNumber) {
        if (!varDecl.getVariables().isEmpty() && matchesType(varDecl.getCommonType(),
            varDecl.getVariable(0).getInitializer().orElse(null), typeNames)) {
          return varDecl.getVariable(0).getNameAsString();
        }
      }
    }

    for (VariableDeclarator varDecl : cu.findAll(VariableDeclarator.class)) {
      if (varDecl.getRange().isPresent() && varDecl.getRange().get().begin.line <= lineNumber
          && varDecl.getRange().get().end.line >= lineNumber
          && matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null), typeNames)) {
        return varDecl.getNameAsString();
      }
    }

    for (FieldDeclaration field : cu.findAll(FieldDeclaration.class)) {
      if (field.getRange().isPresent()) {
        int fieldLine = field.getRange().get().begin.line;
        if (fieldLine <= lineNumber && fieldLine >= lineNumber - 5) {
          if (!field.getVariables().isEmpty() && matchesFieldType(field, typeNames)) {
            return field.getVariable(0).getNameAsString();
          }
        }
      }
    }

    return null;
  }

  @SuppressWarnings("unchecked")
  private static boolean isInlineCreation(ObjectCreationExpr creation) {
    Optional<VariableDeclarator> varDecl = creation.findAncestor(VariableDeclarator.class);
    if (varDecl.isPresent()) {
      return false;
    }

    Optional<MethodCallExpr> methodCall = creation.findAncestor(MethodCallExpr.class);
    if (methodCall.isPresent()) {
      return true;
    }

    Optional<ObjectCreationExpr> parentCreation = creation.findAncestor(ObjectCreationExpr.class);
    return parentCreation.isPresent() && parentCreation.get() != creation;
  }

  /**
   * Checks if the compilation unit contains a class that extends Composite.
   *
   * @param cu the compilation unit to check
   *
   * @return true if the class extends Composite
   */
  public static boolean isCompositeClass(CompilationUnit cu) {
    return cu.findFirst(ClassOrInterfaceDeclaration.class).map(AstFinder::extendsComposite)
        .orElse(false);
  }

  /**
   * Checks whether the bound-component pattern is compatible with the expected component type.
   *
   * <p>
   * Accepts the class when the expected type is the class itself (editing the root composite in its
   * own file) or the {@code Composite<T>} type argument (editing the bound component). A raw
   * {@code Composite} without a type argument is accepted. This guards against stale line numbers
   * routing an edit into an unrelated composite class.
   * </p>
   *
   * @param cu the compilation unit to check
   * @param expectedTypeNames the expected simple type names, or null/empty to skip the check
   *
   * @return true if the bound-component pattern fits the expected type
   */
  public static boolean boundComponentTypeMatches(CompilationUnit cu,
      Collection<String> expectedTypeNames) {
    if (expectedTypeNames == null || expectedTypeNames.isEmpty()) {
      return true;
    }

    return cu.findFirst(ClassOrInterfaceDeclaration.class).map(classDecl -> {
      if (expectedTypeNames.contains(classDecl.getNameAsString())) {
        return true;
      }

      for (ClassOrInterfaceType extendedType : classDecl.getExtendedTypes()) {
        if ("Composite".equals(extendedType.getNameAsString())) {
          return extendedType.getTypeArguments()
              .map(args -> !args.isEmpty() && matchesType(args.get(0), null, expectedTypeNames))
              .orElse(true);
        }
      }

      return false;
    }).orElse(false);
  }

  /**
   * Checks if a class declaration extends Composite.
   *
   * @param classDecl the class declaration to check
   *
   * @return true if the class extends Composite
   */
  public static boolean extendsComposite(ClassOrInterfaceDeclaration classDecl) {
    for (ClassOrInterfaceType extendedType : classDecl.getExtendedTypes()) {
      String typeName = extendedType.getNameAsString();
      if ("Composite".equals(typeName)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Finds the component type declared at the target line.
   *
   * <p>
   * Checks field declarations, local variables, and object creation expressions to find the
   * declared type at the given line.
   * </p>
   *
   * @param cu the compilation unit to search
   * @param target the target context
   *
   * @return the simple type name if found, or null
   */
  public static String findComponentTypeAt(CompilationUnit cu, TargetContext target) {
    int lineNumber = target.getLineNumber();
    // Check field declarations
    for (FieldDeclaration field : cu.findAll(FieldDeclaration.class)) {
      if (field.getRange().isPresent() && field.getRange().get().begin.line <= lineNumber
          && field.getRange().get().end.line >= lineNumber) {
        return field.getCommonType().asString();
      }
    }

    // Check local variable declarations
    for (VariableDeclarationExpr varDecl : cu.findAll(VariableDeclarationExpr.class)) {
      if (varDecl.getRange().isPresent() && varDecl.getRange().get().begin.line <= lineNumber
          && varDecl.getRange().get().end.line >= lineNumber) {
        return varDecl.getCommonType().asString();
      }
    }

    // Check object creation expressions
    for (ObjectCreationExpr creation : cu.findAll(ObjectCreationExpr.class)) {
      if (creation.getRange().isPresent() && creation.getRange().get().begin.line == lineNumber) {
        return creation.getType().getNameAsString();
      }
    }

    return null;
  }

  /**
   * Finds the variable initialized from {@code getBoundComponent()} in a composite class.
   *
   * <p>
   * Composites commonly hold their bound component in an alias such as
   * {@code private FlexLayout self = getBoundComponent();}. When the runtime component is gone the
   * alias declaration is the only anchor left for writing setters on the bound component.
   * </p>
   *
   * @param cu the compilation unit to search
   *
   * @return the alias declarator if one exists
   */
  public static Optional<VariableDeclarator> findBoundComponentAlias(CompilationUnit cu) {
    for (VariableDeclarator varDecl : cu.findAll(VariableDeclarator.class)) {
      Expression init = varDecl.getInitializer().orElse(null);
      if (init instanceof MethodCallExpr mce && "getBoundComponent".equals(mce.getNameAsString())) {
        return Optional.of(varDecl);
      }
    }

    return Optional.empty();
  }

  /**
   * Checks if the constructor uses getBoundComponent() pattern (no variable for the component).
   *
   * <p>
   * Returns true if:
   * </p>
   * <ul>
   * <li>Class extends Composite</li>
   * <li>Either has getBoundComponent() calls in constructor OR has no constructor (we can create
   * one)</li>
   * <li>No variable is assigned to getBoundComponent()</li>
   * </ul>
   *
   * @param cu the compilation unit to check
   * @param target the target context
   *
   * @return true if getBoundComponent() pattern should be used
   */
  public static boolean usesBoundComponentPattern(CompilationUnit cu, TargetContext target) {
    if (!isCompositeClass(cu)) {
      return false;
    }

    if (!boundComponentTypeMatches(cu, acceptedTypes(target))) {
      return false;
    }

    // Check if there's a variable declaration for getBoundComponent
    // e.g., FlexLayout self = getBoundComponent();
    for (VariableDeclarator varDecl : cu.findAll(VariableDeclarator.class)) {
      if (varDecl.getInitializer().isPresent()) {
        var init = varDecl.getInitializer().get();
        if (init instanceof MethodCallExpr mce
            && "getBoundComponent".equals(mce.getNameAsString())) {
          // A variable is assigned to getBoundComponent, so don't use this strategy
          return false;
        }
      }
    }

    // It's a Composite class without a variable for getBoundComponent()
    // We can add getBoundComponent().xxx() calls to the constructor
    return true;
  }

  /**
   * Finds the computed value arguments a setter update would overwrite.
   *
   * <p>
   * A literal or a plain reference is safe to replace, but an argument that computes its value (a
   * method call, an operator expression, a conditional, an object creation or a lambda) carries
   * logic the written literal would erase. The key argument of a key-value call is skipped because
   * it was matched as a literal.
   * </p>
   *
   * @param call the existing setter call about to be updated
   * @param matchKey the key the call was matched by, or null for plain setter calls
   *
   * @return the computed arguments as source text, or null when every argument is safe to replace
   */
  public static String findComputedArguments(MethodCallExpr call, String matchKey) {
    List<String> computed = new ArrayList<>();
    int start = matchKey != null ? 1 : 0;
    for (int i = start; i < call.getArguments().size(); i++) {
      Expression argument = call.getArgument(i);
      if (isComputedExpression(argument) || isCallableScopedReference(argument)) {
        computed.add(argument.toString());
      }
    }

    return computed.isEmpty() ? null : String.join(", ", computed);
  }

  private static boolean isComputedExpression(Expression expression) {
    return expression.findFirst(Node.class,
        node -> node instanceof MethodCallExpr || node instanceof ObjectCreationExpr
            || node instanceof BinaryExpr || node instanceof ConditionalExpr
            || node instanceof LambdaExpr)
        .isPresent();
  }

  // A bare name naming a parameter or local of the enclosing callable carries per-call data; a
  // name resolving elsewhere (a constant, an enum) is a stable reference and stays silent
  private static boolean isCallableScopedReference(Expression expression) {
    if (!(expression instanceof NameExpr name)) {
      return false;
    }

    CallableDeclaration<?> callable = name.findAncestor(CallableDeclaration.class).orElse(null);
    if (callable == null) {
      return false;
    }

    String id = name.getNameAsString();
    boolean isParameter =
        callable.getParameters().stream().anyMatch(p -> p.getNameAsString().equals(id));

    return isParameter || callable.findAll(VariableDeclarationExpr.class).stream()
        .flatMap(decl -> decl.getVariables().stream())
        .anyMatch(varDecl -> varDecl.getNameAsString().equals(id));
  }
}
