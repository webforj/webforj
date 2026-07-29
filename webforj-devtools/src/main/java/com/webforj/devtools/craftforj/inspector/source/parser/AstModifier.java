package com.webforj.devtools.craftforj.inspector.source.parser;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange.ItemPosition;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Utility for modifying JavaParser AST.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class AstModifier {

  private static final String GET_BOUND_COMPONENT = "getBoundComponent";

  private AstModifier() {}

  /**
   * Creates a setter statement.
   *
   * <p>
   * When the source change carries an accessor, the setter is scoped by it:
   * {@code variable.getSearch().setPlaceholder("value")}.
   * </p>
   *
   * @param varName the variable name to call the setter on
   * @param sourceChange the source change containing the method name and arguments
   *
   * @return the setter statement
   */
  public static Statement createSetterStatement(String varName, SourceChange sourceChange) {
    NodeList<Expression> args = new NodeList<>(sourceChange.getArguments());
    Expression scope = new NameExpr(varName);
    if (sourceChange.getAccessor() != null) {
      scope = new MethodCallExpr(scope, sourceChange.getAccessor());
    }

    MethodCallExpr setterCall = new MethodCallExpr(scope, sourceChange.getMethodName(), args);

    return new ExpressionStmt(setterCall);
  }

  /**
   * Creates a setter statement using getBoundComponent() as the scope.
   *
   * <p>
   * This generates code like: {@code getBoundComponent().setText("value")}, or
   * {@code getBoundComponent().getSearch().setPlaceholder("value")} when the source change carries
   * an accessor.
   * </p>
   *
   * @param sourceChange the source change containing the method name and arguments
   *
   * @return the setter statement
   */
  public static Statement createBoundComponentSetterStatement(SourceChange sourceChange) {
    NodeList<Expression> args = new NodeList<>(sourceChange.getArguments());
    Expression scope = new MethodCallExpr(GET_BOUND_COMPONENT);
    if (sourceChange.getAccessor() != null) {
      scope = new MethodCallExpr(scope, sourceChange.getAccessor());
    }

    MethodCallExpr setterCall = new MethodCallExpr(scope, sourceChange.getMethodName(), args);

    return new ExpressionStmt(setterCall);
  }

  /**
   * Removes a method call on a variable.
   *
   * @param cu the compilation unit to search
   * @param varName the variable name to find method calls on
   * @param methodName the method name to remove
   *
   * @return true if a method call was removed
   */
  public static boolean removeMethodCall(CompilationUnit cu, String varName, String methodName) {
    return removeMethodCall(cu, varName, methodName, null);
  }

  /**
   * Removes a method call on a variable, scoped by an accessor.
   *
   * @param cu the compilation unit to search
   * @param varName the variable name to find method calls on
   * @param methodName the method name to remove
   * @param accessor the accessor method scoping the call, or null for direct calls
   *
   * @return true if a method call was removed
   */
  @SuppressWarnings("unchecked")
  public static boolean removeMethodCall(CompilationUnit cu, String varName, String methodName,
      String accessor) {
    Statement toRemove = cu.findAll(MethodCallExpr.class).stream()
        .filter(mc -> mc.getNameAsString().equals(methodName) && isMethodCallOnVariable(mc, varName)
            && Objects.equals(getDirectAccessor(mc), accessor))
        .findFirst().flatMap(mc -> mc.findAncestor(Statement.class))
        .filter(stmt -> stmt.getParentNode().isPresent()).orElse(null);

    if (toRemove != null) {
      toRemove.remove();
      return true;
    }

    return false;
  }

  /**
   * Updates an existing setter call if found.
   *
   * <p>
   * The search root bounds where existing calls are looked up. Callers writing into an
   * initialization block must pass that block, never the whole file: a same-named call inside an
   * unrelated method (a public API body) would otherwise be rewritten while the component's own
   * initialization keeps running unchanged, and the save lands in dead code.
   * </p>
   *
   * @param searchRoot the node whose subtree is searched for the existing call
   * @param varName the variable name to find setter calls on
   * @param sourceChange the source change containing the new argument
   *
   * @return true if an existing setter was updated
   */
  public static boolean updateExistingSetterCall(Node searchRoot, String varName,
      SourceChange sourceChange) {
    MethodCallExpr lastMatch = findExistingSetterCall(searchRoot, varName, sourceChange);
    if (lastMatch != null) {
      sourceChange.setReplacedComputedExpression(
          AstFinder.findComputedArguments(lastMatch, sourceChange.getMatchKey()));
      lastMatch.getArguments().clear();
      for (Expression arg : sourceChange.getArguments()) {
        lastMatch.addArgument(arg);
      }

      return true;
    }

    return false;
  }

  /**
   * Finds the existing setter call a source change would update, without modifying it.
   *
   * <p>
   * A null variable name matches the {@code getBoundComponent()} pattern instead of a variable.
   * When several calls match, the last one wins, mirroring the update behavior.
   * </p>
   *
   * @param searchRoot the node whose subtree is searched
   * @param varName the variable name the setter is called on, or null for getBoundComponent()
   * @param sourceChange the source change describing the call
   *
   * @return the matching call, or null when the change would insert a new call
   */
  public static MethodCallExpr findExistingSetterCall(Node searchRoot, String varName,
      SourceChange sourceChange) {
    MethodCallExpr lastMatch = null;
    String matchKey = sourceChange.getMatchKey();

    for (MethodCallExpr methodCall : searchRoot.findAll(MethodCallExpr.class)) {
      boolean scopeMatches = varName != null ? isMethodCallOnVariable(methodCall, varName)
          : isMethodCallOnBoundComponent(methodCall);
      if (methodCall.getNameAsString().equals(sourceChange.getMethodName()) && scopeMatches
          && Objects.equals(getDirectAccessor(methodCall), sourceChange.getAccessor())) {
        if (matchKey != null) {
          // Match by key for key-value methods
          if (methodCall.getArguments().size() >= 1) {
            String existingKey = methodCall.getArgument(0).toString().replace("\"", "");
            if (existingKey.equals(matchKey)) {
              lastMatch = methodCall;
            }
          }
        } else {
          lastMatch = methodCall;
        }
      }
    }

    return lastMatch;
  }

  /**
   * Checks if a method call is on a specific variable.
   *
   * @param methodCall the method call expression to check
   * @param varName the variable name to match
   *
   * @return true if the method call is on the specified variable
   */
  public static boolean isMethodCallOnVariable(MethodCallExpr methodCall, String varName) {
    Expression current = methodCall;

    while (current instanceof MethodCallExpr mce) {
      if (mce.getScope().isEmpty()) {
        return false;
      }

      current = mce.getScope().get();
    }

    if (current instanceof NameExpr nameExpr) {
      return nameExpr.getNameAsString().equals(varName);
    }

    // A field referenced from its own class reads as this.field
    if (current instanceof FieldAccessExpr fieldAccess
        && fieldAccess.getScope() instanceof ThisExpr) {
      return fieldAccess.getNameAsString().equals(varName);
    }

    return false;
  }

  /**
   * Checks if a method call is on getBoundComponent().
   *
   * <p>
   * Matches patterns like: {@code getBoundComponent().setText(...)} and accessor-scoped patterns
   * like {@code getBoundComponent().getSearch().setPlaceholder(...)}.
   * </p>
   *
   * @param methodCall the method call expression to check
   *
   * @return true if the method call is on getBoundComponent()
   */
  public static boolean isMethodCallOnBoundComponent(MethodCallExpr methodCall) {
    if (methodCall.getScope().isEmpty()) {
      return false;
    }

    Expression scope = methodCall.getScope().get();
    if (scope instanceof MethodCallExpr mce) {
      if (GET_BOUND_COMPONENT.equals(mce.getNameAsString()) && mce.getScope().isEmpty()) {
        return true;
      }

      // Accessor-scoped: getBoundComponent().getSearch().setX(...)
      if (mce.getArguments().isEmpty() && mce.getScope().isPresent()
          && mce.getScope().get() instanceof MethodCallExpr inner
          && GET_BOUND_COMPONENT.equals(inner.getNameAsString()) && inner.getScope().isEmpty()) {
        return true;
      }
    }

    return false;
  }

  /**
   * Updates an existing setter call on getBoundComponent() if found.
   *
   * @param searchRoot the node whose subtree is searched for the existing call
   * @param sourceChange the source change containing the new argument
   *
   * @return true if an existing setter was updated
   */
  public static boolean updateExistingBoundComponentSetterCall(Node searchRoot,
      SourceChange sourceChange) {
    return updateExistingSetterCall(searchRoot, null, sourceChange);
  }

  /**
   * Removes a method call on getBoundComponent().
   *
   * @param cu the compilation unit to search
   * @param methodName the method name to remove
   *
   * @return true if a method call was removed
   */
  public static boolean removeBoundComponentMethodCall(CompilationUnit cu, String methodName) {
    return removeBoundComponentMethodCall(cu, methodName, null);
  }

  /**
   * Removes a method call on getBoundComponent(), scoped by an accessor.
   *
   * @param cu the compilation unit to search
   * @param methodName the method name to remove
   * @param accessor the accessor method scoping the call, or null for direct calls
   *
   * @return true if a method call was removed
   */
  @SuppressWarnings("unchecked")
  public static boolean removeBoundComponentMethodCall(CompilationUnit cu, String methodName,
      String accessor) {
    Statement toRemove = cu.findAll(MethodCallExpr.class).stream()
        .filter(mc -> mc.getNameAsString().equals(methodName) && isMethodCallOnBoundComponent(mc)
            && Objects.equals(getDirectAccessor(mc), accessor))
        .findFirst().flatMap(mc -> mc.findAncestor(Statement.class))
        .filter(stmt -> stmt.getParentNode().isPresent()).orElse(null);

    if (toRemove != null) {
      toRemove.remove();
      return true;
    }

    return false;
  }

  /**
   * Updates an existing parent-scoped item call if found.
   *
   * <p>
   * Matches calls by method name, scope, and the item variable at the position declared by the
   * source change. Arity is part of the match so overloads (breakpoint variants, container-level
   * overloads of the same method) are never touched. When the matched call is a varargs call
   * listing several items, the item is detached from it and {@code false} is returned so the caller
   * inserts a dedicated single-item call.
   * </p>
   *
   * @param cu the compilation unit to search
   * @param scopeMatcher predicate identifying calls on the parent scope
   * @param sourceChange the item source change containing the new arguments
   *
   * @return true if an existing call was updated in place
   */
  public static boolean updateExistingItemCall(CompilationUnit cu,
      Predicate<MethodCallExpr> scopeMatcher, SourceChange sourceChange) {
    MethodCallExpr lastMatch = null;

    for (MethodCallExpr methodCall : cu.findAll(MethodCallExpr.class)) {
      if (methodCall.getNameAsString().equals(sourceChange.getMethodName())
          && scopeMatcher.test(methodCall) && matchesItemCall(methodCall, sourceChange.getItemRef(),
              sourceChange.getItemPosition(), sourceChange.getArguments().size())) {
        lastMatch = methodCall;
      }
    }

    if (lastMatch == null) {
      return false;
    }

    if (sourceChange.getItemPosition() == ItemPosition.LAST
        && lastMatch.getArguments().size() > sourceChange.getArguments().size()) {
      detachItemArguments(lastMatch, sourceChange.getItemRef(),
          sourceChange.getArguments().size() - 1);

      return false;
    }

    lastMatch.getArguments().clear();
    for (Expression arg : sourceChange.getArguments()) {
      lastMatch.addArgument(arg);
    }

    return true;
  }

  /**
   * Removes a parent-scoped item call for the given item.
   *
   * <p>
   * When the matched call is a varargs call listing several items, only the item argument is
   * detached and the call is kept for the remaining items. Otherwise the whole statement is
   * removed.
   * </p>
   *
   * @param cu the compilation unit to search
   * @param scopeMatcher predicate identifying calls on the parent scope
   * @param methodName the method name to remove
   * @param itemRef the item variable name
   * @param itemPosition the position of the item argument
   * @param expectedArgCount the argument count of a devtools-generated call for this method
   *
   * @return true if a call was removed or the item was detached from a shared call
   */
  public static boolean removeItemCall(CompilationUnit cu, Predicate<MethodCallExpr> scopeMatcher,
      String methodName, String itemRef, ItemPosition itemPosition, int expectedArgCount) {
    MethodCallExpr lastMatch = null;

    for (MethodCallExpr methodCall : cu.findAll(MethodCallExpr.class)) {
      if (methodCall.getNameAsString().equals(methodName) && scopeMatcher.test(methodCall)
          && matchesItemCall(methodCall, itemRef, itemPosition, expectedArgCount)) {
        lastMatch = methodCall;
      }
    }

    if (lastMatch == null) {
      return false;
    }

    if (itemPosition == ItemPosition.LAST && lastMatch.getArguments().size() > expectedArgCount) {
      detachItemArguments(lastMatch, itemRef, expectedArgCount - 1);
      return true;
    }

    return lastMatch.findAncestor(Statement.class).filter(stmt -> stmt.getParentNode().isPresent())
        .map(stmt -> {
          stmt.remove();
          return true;
        }).orElse(false);
  }

  /**
   * Finds the insertion point for a parent-scoped item call.
   *
   * <p>
   * Parent APIs like {@code setItemGrow} validate that the item is already a child of the layout,
   * so the generated call must come after the statement that adds the item to the parent. The
   * insertion point is therefore after the last statement that either calls a method on the parent
   * scope or references the item variable (its declaration, {@code parent.add(item)}, or a
   * constructor call receiving it).
   * </p>
   *
   * @param block the block statement to search
   * @param scopeMatcher predicate identifying calls on the parent scope
   * @param itemRef the item variable name
   *
   * @return the index to insert after, or -1 if neither the parent nor the item is referenced
   */
  public static int findInsertionPointForItemCall(BlockStmt block,
      Predicate<MethodCallExpr> scopeMatcher, String itemRef) {
    int lastRelevant = -1;

    for (int i = 0; i < block.getStatements().size(); i++) {
      Statement stmt = block.getStatement(i);

      boolean scopeHit = stmt.findAll(MethodCallExpr.class).stream().anyMatch(scopeMatcher);
      boolean itemHit =
          stmt.findAll(NameExpr.class).stream().anyMatch(n -> n.getNameAsString().equals(itemRef))
              || stmt.findAll(VariableDeclarator.class).stream()
                  .anyMatch(v -> v.getNameAsString().equals(itemRef));

      if (scopeHit || itemHit) {
        lastRelevant = i;
      }
    }

    return lastRelevant;
  }

  private static boolean matchesItemCall(MethodCallExpr call, String itemRef,
      ItemPosition itemPosition, int expectedArgCount) {
    List<Expression> args = call.getArguments();

    if (itemPosition == ItemPosition.FIRST) {
      return args.size() == expectedArgCount && !args.isEmpty() && isItemName(args.get(0), itemRef);
    }

    int valueArgCount = expectedArgCount - 1;
    if (args.size() <= valueArgCount) {
      return false;
    }

    for (int i = valueArgCount; i < args.size(); i++) {
      if (isItemName(args.get(i), itemRef)) {
        return true;
      }
    }

    return false;
  }

  private static void detachItemArguments(MethodCallExpr call, String itemRef, int fromIndex) {
    List<Expression> toDetach = call.getArguments().stream().skip(fromIndex)
        .filter(arg -> isItemName(arg, itemRef)).toList();
    toDetach.forEach(call.getArguments()::remove);
  }

  private static boolean isItemName(Expression expression, String itemRef) {
    return expression instanceof NameExpr nameExpr && nameExpr.getNameAsString().equals(itemRef);
  }

  /**
   * Gets the accessor method that directly scopes a setter call.
   *
   * <p>
   * An accessor is a no-argument getter used as the direct scope of the call, e.g. "getSearch" in
   * {@code nav.getSearch().setPlaceholder(...)}. Fluent setter chains and getBoundComponent()
   * scopes are not accessors.
   * </p>
   *
   * @param methodCall the method call expression to check
   *
   * @return the accessor method name, or null if the call is not accessor-scoped
   */
  public static String getDirectAccessor(MethodCallExpr methodCall) {
    if (methodCall.getScope().isEmpty()) {
      return null;
    }

    Expression scope = methodCall.getScope().get();
    if (scope instanceof MethodCallExpr mce && mce.getArguments().isEmpty()
        && mce.getNameAsString().startsWith("get")
        && !GET_BOUND_COMPONENT.equals(mce.getNameAsString())) {
      return mce.getNameAsString();
    }

    return null;
  }

  /**
   * Finds the insertion point after consecutive getBoundComponent() method calls.
   *
   * @param block the block statement to search
   *
   * @return the index to insert after, or -1 if no method calls found
   */
  public static int findInsertionPointForBoundComponent(BlockStmt block) {
    int firstMethodCallIndex = -1;

    for (int i = 0; i < block.getStatements().size(); i++) {
      Statement stmt = block.getStatement(i);

      boolean isMethodOnBoundComponent = stmt.findAll(MethodCallExpr.class).stream()
          .anyMatch(AstModifier::isMethodCallOnBoundComponent);

      if (isMethodOnBoundComponent) {
        if (firstMethodCallIndex == -1) {
          firstMethodCallIndex = i;
        }
      }
    }

    if (firstMethodCallIndex >= 0) {
      int insertAfter = firstMethodCallIndex;
      for (int i = firstMethodCallIndex + 1; i < block.getStatements().size(); i++) {
        Statement stmt = block.getStatement(i);
        boolean isMethodOnBoundComponent = stmt.findAll(MethodCallExpr.class).stream()
            .anyMatch(AstModifier::isMethodCallOnBoundComponent);
        if (isMethodOnBoundComponent) {
          insertAfter = i;
        } else {
          break;
        }
      }

      return insertAfter;
    }

    return -1;
  }

  /**
   * Finds the insertion point after consecutive method calls on a variable.
   *
   * @param block the block statement to search
   * @param varName the variable name to find method calls on
   *
   * @return the index to insert after, or -1 if no method calls found
   */
  public static int findInsertionPointForVariable(BlockStmt block, String varName) {
    int firstMethodCallIndex = -1;

    for (int i = 0; i < block.getStatements().size(); i++) {
      Statement stmt = block.getStatement(i);

      boolean isMethodOnVar = stmt.findAll(MethodCallExpr.class).stream()
          .anyMatch(methodCall -> isMethodCallOnVariable(methodCall, varName));

      if (isMethodOnVar) {
        if (firstMethodCallIndex == -1) {
          firstMethodCallIndex = i;
        }
      }
    }

    if (firstMethodCallIndex >= 0) {
      int insertAfter = firstMethodCallIndex;
      for (int i = firstMethodCallIndex + 1; i < block.getStatements().size(); i++) {
        Statement stmt = block.getStatement(i);
        boolean isMethodOnVar = stmt.findAll(MethodCallExpr.class).stream()
            .anyMatch(methodCall -> isMethodCallOnVariable(methodCall, varName));
        if (isMethodOnVar) {
          insertAfter = i;
        } else {
          break;
        }
      }

      return insertAfter;
    }

    return -1;
  }

  /**
   * Adds a setter to constructor with smart positioning.
   *
   * @param classDecl the class declaration to add the setter to
   * @param varName the variable name for positioning
   * @param setterStatement the setter statement to add
   */
  public static void addSetterWithSmartPosition(ClassOrInterfaceDeclaration classDecl,
      String varName, Statement setterStatement) {
    List<ConstructorDeclaration> constructors = classDecl.getConstructors();
    if (constructors.isEmpty()) {
      ConstructorDeclaration ctor = classDecl.addConstructor();
      ctor.setBody(new BlockStmt());
      ctor.getBody().addStatement(setterStatement);

      return;
    }

    BlockStmt block = constructors.get(0).getBody();
    int insertAfterIndex = findInsertionPointForVariable(block, varName);

    if (insertAfterIndex >= 0) {
      block.addStatement(insertAfterIndex + 1, setterStatement);
    } else {
      block.addStatement(setterStatement);
    }
  }

  /**
   * Adds a setter to the first constructor.
   *
   * @param classDecl the class declaration to add the setter to
   * @param setterStatement the setter statement to add
   */
  public static void addSetterToConstructor(ClassOrInterfaceDeclaration classDecl,
      Statement setterStatement) {
    List<ConstructorDeclaration> constructors = classDecl.getConstructors();
    if (constructors.isEmpty()) {
      ConstructorDeclaration ctor = classDecl.addConstructor();
      ctor.setBody(new BlockStmt());
      ctor.getBody().addStatement(setterStatement);
    } else {
      constructors.get(0).getBody().addStatement(setterStatement);
    }
  }

  /**
   * Generates a unique variable name.
   *
   * @param baseName the base name to use
   * @param block the block statement to check for existing names
   *
   * @return a unique variable name
   */
  @SuppressWarnings("unchecked")
  public static String generateFreeVariableName(String baseName, BlockStmt block) {
    Set<String> usedNames = new HashSet<>();

    block.findAll(VariableDeclarator.class).forEach(v -> usedNames.add(v.getNameAsString()));

    block.findAncestor(ClassOrInterfaceDeclaration.class).ifPresent(classDecl -> {
      classDecl.getFields().forEach(field -> {
        field.getVariables().forEach(v -> usedNames.add(v.getNameAsString()));
      });
    });

    if (!usedNames.contains(baseName)) {
      return baseName;
    }

    int suffix = 2;
    while (usedNames.contains(baseName + suffix)) {
      suffix++;
    }

    return baseName + suffix;
  }

  /**
   * Adds an import if it doesn't already exist.
   *
   * @param cu the compilation unit
   * @param qualifiedName the fully qualified class name to import
   */
  public static void addImportIfNotExists(CompilationUnit cu, String qualifiedName) {
    boolean exists =
        cu.getImports().stream().anyMatch(imp -> imp.getNameAsString().equals(qualifiedName));

    if (!exists) {
      cu.addImport(new ImportDeclaration(qualifiedName, false, false));
    }
  }

  /**
   * Extracts an inline expression to a variable and adds setter calls.
   *
   * <p>
   * This transforms code like {@code add(new Button())} or {@code add(Icon.create("x"))} into:
   * </p>
   *
   * <pre>
   * Button button = new Button();
   * button.setText("Hello");
   * button.setVisible(true);
   * add(button);
   * </pre>
   *
   * @param expr the expression to extract (ObjectCreationExpr or MethodCallExpr)
   * @param sourceChanges the setters to add
   * @param typeName the type name for the variable
   *
   * @return true if extraction succeeded
   */
  @SuppressWarnings("unchecked")
  public static boolean extractToVariableAndAddSetters(Expression expr,
      List<SourceChange> sourceChanges, String typeName) {
    if (sourceChanges == null || sourceChanges.isEmpty()) {
      return false;
    }

    BlockStmt block = expr.findAncestor(BlockStmt.class).orElse(null);
    if (block == null) {
      return false;
    }

    String varName = extractToVariable(expr, typeName);
    if (varName == null) {
      return false;
    }

    int declIndex = indexOfDeclaration(block, varName);
    int setterIndex = declIndex + 1;
    for (SourceChange sourceChange : sourceChanges) {
      Statement setterStmt = createSetterStatement(varName, sourceChange);
      block.addStatement(setterIndex++, setterStmt);
    }

    return true;
  }

  /**
   * Extracts an inline expression to a local variable.
   *
   * <p>
   * This transforms code like {@code add(new Button())} into {@code Button button = new Button();
   * add(button);} and returns the generated variable name so callers can reference the component,
   * e.g. when a parent-scoped item call needs a name for an inline-created child.
   * </p>
   *
   * @param expr the expression to extract (ObjectCreationExpr or MethodCallExpr)
   * @param typeName the type name for the variable
   *
   * @return the generated variable name, or null if extraction is not possible
   */
  @SuppressWarnings("unchecked")
  public static String extractToVariable(Expression expr, String typeName) {
    BlockStmt block = expr.findAncestor(BlockStmt.class).orElse(null);
    if (block == null) {
      return null;
    }

    Statement containingStmt = expr.findAncestor(Statement.class).orElse(null);
    if (containingStmt == null) {
      return null;
    }

    int insertIndex = block.getStatements().indexOf(containingStmt);
    if (insertIndex < 0) {
      return null;
    }

    String varName = generateFreeVariableName(typeName.toLowerCase(), block);

    VariableDeclarator varDeclarator =
        new VariableDeclarator(StaticJavaParser.parseType(typeName), varName, expr.clone());
    VariableDeclarationExpr varDecl = new VariableDeclarationExpr(varDeclarator);
    ExpressionStmt declStmt = new ExpressionStmt(varDecl);

    block.addStatement(insertIndex, declStmt);
    expr.replace(new NameExpr(varName));

    return varName;
  }

  private static int indexOfDeclaration(BlockStmt block, String varName) {
    for (int i = 0; i < block.getStatements().size(); i++) {
      boolean declares = block.getStatement(i).findAll(VariableDeclarator.class).stream()
          .anyMatch(v -> v.getNameAsString().equals(varName));
      if (declares) {
        return i;
      }
    }

    return block.getStatements().size() - 1;
  }

  /**
   * Adds setter calls for a variable.
   *
   * <p>
   * Updates existing setters if found, otherwise adds new ones after the variable's method calls.
   * </p>
   *
   * @param cu the compilation unit
   * @param block the block containing the variable
   * @param varName the variable name
   * @param sourceChanges the setters to add
   */
  public static void addSettersForVariable(CompilationUnit cu, BlockStmt block, String varName,
      List<SourceChange> sourceChanges) {
    for (SourceChange sourceChange : sourceChanges) {
      if (sourceChange.getItemRef() != null) {
        Predicate<MethodCallExpr> scopeMatcher = mc -> isMethodCallOnVariable(mc, varName);
        if (!updateExistingItemCall(cu, scopeMatcher, sourceChange)) {
          Statement setterStmt = createSetterStatement(varName, sourceChange);
          insertItemCall(block, scopeMatcher, sourceChange.getItemRef(), setterStmt);
        }
        continue;
      }

      if (!updateExistingSetterCall(block, varName, sourceChange)) {
        Statement setterStmt = createSetterStatement(varName, sourceChange);
        int insertAfterIndex = findInsertionPointForVariable(block, varName);
        if (insertAfterIndex >= 0) {
          block.addStatement(insertAfterIndex + 1, setterStmt);
        } else {
          block.addStatement(setterStmt);
        }
      }
    }
  }

  private static void insertItemCall(BlockStmt block, Predicate<MethodCallExpr> scopeMatcher,
      String itemRef, Statement setterStmt) {
    int insertAfterIndex = findInsertionPointForItemCall(block, scopeMatcher, itemRef);
    if (insertAfterIndex >= 0) {
      block.addStatement(insertAfterIndex + 1, setterStmt);
    } else {
      block.addStatement(setterStmt);
    }
  }

  /**
   * Adds setter calls for getBoundComponent().
   *
   * <p>
   * Updates existing setters if found, otherwise adds new ones in the constructor.
   * </p>
   *
   * @param cu the compilation unit
   * @param classDecl the class declaration
   * @param sourceChanges the setters to add
   */
  public static void addSettersForBoundComponent(CompilationUnit cu,
      ClassOrInterfaceDeclaration classDecl, List<SourceChange> sourceChanges) {
    List<ConstructorDeclaration> constructors = classDecl.getConstructors();
    BlockStmt block;

    if (constructors.isEmpty()) {
      ConstructorDeclaration ctor = classDecl.addConstructor();
      ctor.setBody(new BlockStmt());
      block = ctor.getBody();
    } else {
      block = constructors.get(0).getBody();
    }

    for (SourceChange sourceChange : sourceChanges) {
      if (sourceChange.getItemRef() != null) {
        Predicate<MethodCallExpr> scopeMatcher = AstModifier::isMethodCallOnBoundComponent;
        if (!updateExistingItemCall(cu, scopeMatcher, sourceChange)) {
          Statement setterStmt = createBoundComponentSetterStatement(sourceChange);
          insertItemCall(block, scopeMatcher, sourceChange.getItemRef(), setterStmt);
        }
        continue;
      }

      if (!updateExistingBoundComponentSetterCall(block, sourceChange)) {
        Statement setterStmt = createBoundComponentSetterStatement(sourceChange);
        int insertAfterIndex = findInsertionPointForBoundComponent(block);
        if (insertAfterIndex >= 0) {
          block.addStatement(insertAfterIndex + 1, setterStmt);
        } else {
          block.addStatement(setterStmt);
        }
      }
    }
  }
}
