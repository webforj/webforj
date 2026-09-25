package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.InitializerDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import com.webforj.devtools.craftforj.source.model.SourceChange.ItemPosition;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
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
    Predicate<MethodCallExpr> scopeMatcher =
        varName != null ? call -> isMethodCallOnVariable(call, varName)
            : AstModifier::isMethodCallOnBoundComponent;
    return updateMatchingSetterCall(searchRoot, scopeMatcher, sourceChange);
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
    Predicate<MethodCallExpr> scopeMatcher =
        varName != null ? call -> isMethodCallOnVariable(call, varName)
            : AstModifier::isMethodCallOnBoundComponent;
    return findMatchingSetterCall(searchRoot, scopeMatcher, sourceChange);
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
    return VariableReferences.isScopeOnBoundComponent(methodCall.getScope().orElse(null));
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
    return updateMatchingSetterCall(searchRoot, AstModifier::isMethodCallOnBoundComponent,
        sourceChange);
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
    return updateMatchingSetterCall(cu, scopeMatcher, sourceChange);
  }

  /**
   * Removes a parent-scoped item call for the given item.
   *
   * <p>
   * When the matched call is a varargs call listing several items, only the item argument is
   * detached and the call is kept for the remaining items. Otherwise the call is removed,
   * preserving other calls in a fluent chain.
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
    SourceChange change = SourceChange.builder().removeMethodCall(methodName)
        .itemRef(itemRef, itemPosition, expectedArgCount).build();
    MethodCallExpr lastMatch = findMatchingSetterCall(cu, scopeMatcher, change);

    if (lastMatch == null) {
      return false;
    }

    if (!detachSharedItem(lastMatch, change)) {
      removeCallExpression(lastMatch);
    }
    return true;
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

  private static boolean matchesItemCall(MethodCallExpr call, SourceChange change) {
    List<Expression> args = call.getArguments();
    ItemPosition itemPosition = change.getItemPosition();
    int expectedArgCount = change.getItemArgumentCount();

    if (itemPosition == ItemPosition.FIRST) {
      return args.size() == expectedArgCount && !args.isEmpty()
          && isItemReference(args.get(0), change);
    }

    int valueArgCount = expectedArgCount - 1;
    if (args.size() <= valueArgCount) {
      return false;
    }

    for (int i = valueArgCount; i < args.size(); i++) {
      if (isItemReference(args.get(i), change)) {
        return true;
      }
    }

    return false;
  }

  private static void detachItemArguments(MethodCallExpr call, SourceChange change, int fromIndex) {
    List<Expression> toDetach = call.getArguments().stream().skip(fromIndex)
        .filter(arg -> isItemReference(arg, change)).toList();
    toDetach.forEach(call.getArguments()::remove);
  }

  private static boolean isItemReference(Expression expression, SourceChange change) {
    if (change.getItemDeclaration() != null) {
      return VariableReferences.isReferenceToVariable(expression, change.getItemDeclaration());
    }
    return expression instanceof NameExpr nameExpr
        && nameExpr.getNameAsString().equals(change.getItemRef());
  }

  /**
   * Gets the accessor owning a setter call, including fluent setter chains.
   *
   * <p>
   * An accessor is a no-argument getter such as "getSearch" in
   * {@code nav.getSearch().setFieldVisible(true).setPlaceholder(...)}. A chain without an accessor
   * and getBoundComponent() scopes return null.
   * </p>
   *
   * @param methodCall the method call expression to check
   *
   * @return the accessor method name, or null if the call is not accessor-scoped
   */
  public static String getDirectAccessor(MethodCallExpr methodCall) {
    return VariableReferences.getAccessor(methodCall.getScope().orElse(null));
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

    Map<SourceChange, Statement> helpers = new IdentityHashMap<>();
    expr.findAncestor(BlockStmt.class).ifPresent(block -> sourceChanges.forEach(change -> {
      Statement helper = HelperEffects.findLastWrite(block,
          receiver -> VariableReferences.isScopeOnExpression(receiver, expr), change);
      if (helper != null) {
        helpers.put(change, helper);
      }
    }));

    for (SourceChange change : sourceChanges) {
      if (change.isRemoval() || helpers.containsKey(change)) {
        removeChainedCalls(expr, change);
      }
    }
    Expression chain = expr;
    while (chain.getParentNode().orElse(null) instanceof MethodCallExpr call
        && call.getScope().orElse(null) == chain) {
      chain = call;
    }
    List<SourceChange> writes = new ArrayList<>();
    for (SourceChange change : sourceChanges) {
      if (!change.isRemoval() && (helpers.containsKey(change) || !updateMatchingSetterCall(chain,
          call -> VariableReferences.isScopeOnExpression(call, expr), change))) {
        writes.add(change);
      }
    }
    if (writes.isEmpty()) {
      return true;
    }

    BlockStmt block = expr.findAncestor(BlockStmt.class).orElse(null);
    if (block == null) {
      return false;
    }

    String varName = extractToVariable(chain, typeName);
    if (varName == null) {
      return false;
    }

    int declIndex = indexOfDeclaration(block, varName);
    int setterIndex = declIndex + 1;
    for (SourceChange sourceChange : writes) {
      Statement setterStmt = createSetterStatement(varName, sourceChange);
      Statement helper = helpers.get(sourceChange);
      if (helper != null) {
        block.addStatement(VariableReferences.indexOfSame(block.getStatements(), helper) + 1,
            setterStmt);
      } else {
        block.addStatement(setterIndex++, setterStmt);
      }
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

    int insertIndex = VariableReferences.indexOfSame(block.getStatements(), containingStmt);
    if (insertIndex < 0) {
      return null;
    }

    var declarationType = StaticJavaParser.parseClassOrInterfaceType(typeName);
    String varName =
        generateFreeVariableName(declarationType.getNameAsString().toLowerCase(Locale.ROOT), block);

    VariableDeclarator varDeclarator =
        new VariableDeclarator(declarationType, varName, expr.clone());
    VariableDeclarationExpr varDecl = new VariableDeclarationExpr(varDeclarator);
    ExpressionStmt declStmt = new ExpressionStmt(varDecl);

    expr.replace(new NameExpr(varName));
    block.setStatement(insertIndex, declStmt);
    block.addStatement(insertIndex + 1, containingStmt);

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
    applyVariableSetters(cu, block, varName, sourceChanges, null, null);
  }

  /**
   * Writes properties only on references to the selected variable declaration.
   *
   * @param cu the compilation unit
   * @param block the initialization block
   * @param variable the selected field or local declaration
   * @param sourceChanges the setters to update, insert or remove
   */
  public static void addSettersForDeclaration(CompilationUnit cu, BlockStmt block,
      VariableDeclarator variable, List<SourceChange> sourceChanges) {
    for (SourceChange change : sourceChanges) {
      requireStableBinding(block, variable, change.getAccessor());
    }
    applyVariableSetters(cu, block, variable.getNameAsString(), sourceChanges, variable,
        receiver -> VariableReferences.isScopeOnVariable(receiver, variable));
  }

  /**
   * Checks whether an initialization block contains a write to the selected declaration.
   *
   * @param block the initialization block
   * @param variable the selected declaration
   * @param change the property write to match
   * @return true when the block contains a matching initialization call
   */
  public static boolean hasSetterForDeclaration(BlockStmt block, VariableDeclarator variable,
      SourceChange change) {
    HelperEffects.findLastWrite(block,
        receiver -> VariableReferences.isScopeOnVariable(receiver, variable), change);
    return findMatchingSetterCall(block,
        call -> VariableReferences.isCallOnVariable(call, variable), change) != null;
  }

  /**
   * Updates or removes an existing setter in a declaration's initializer expression.
   *
   * @param variable the declaration whose component receives the initializer calls
   * @param change the requested setter operation
   * @return true when a matching initializer call was changed
   */
  public static boolean updateInitializerSetter(VariableDeclarator variable, SourceChange change) {
    Predicate<MethodCallExpr> matcher = call -> VariableReferences.isCallOnVariable(call, variable);
    if (change.isRemoval()) {
      boolean found = findMatchingSetterCall(variable, matcher, change) != null;
      removeExistingCalls(variable, matcher, change);
      return found;
    }
    return updateMatchingSetterCall(variable, matcher, change);
  }

  private static void insertItemCall(BlockStmt block, Predicate<MethodCallExpr> scopeMatcher,
      SourceChange change, Statement setterStmt) {
    int insertAfterIndex = findInsertionPointForItemCall(block, scopeMatcher, change.getItemRef());
    if (insertAfterIndex >= 0) {
      block.addStatement(insertAfterIndex + 1, setterStmt);
    } else {
      block.addStatement(setterStmt);
    }
    qualifyItemReference(setterStmt.asExpressionStmt().getExpression().asMethodCallExpr(), change);
  }

  /**
   * Adds setter calls for getBoundComponent().
   *
   * <p>
   * Updates existing initialization setters and covers every independent constructor path.
   * </p>
   *
   * @param cu the compilation unit
   * @param classDecl the class declaration
   * @param sourceChanges the setters to add
   */
  public static void addSettersForBoundComponent(CompilationUnit cu,
      ClassOrInterfaceDeclaration classDecl, List<SourceChange> sourceChanges) {
    List<ConstructorDeclaration> constructors = classDecl.getConstructors();
    for (SourceChange change : sourceChanges) {
      boolean initialized = false;
      for (BodyDeclaration<?> member : classDecl.getMembers()) {
        if (!(member instanceof InitializerDeclaration initializer) || initializer.isStatic()) {
          continue;
        }
        Statement helper = HelperEffects.findLastWrite(initializer.getBody(),
            VariableReferences::isScopeOnBoundComponent, change);
        if (findMatchingSetterCall(initializer.getBody(), AstModifier::isMethodCallOnBoundComponent,
            change) != null) {
          applyBoundComponentSetter(initializer.getBody(), change, helper);
          initialized = true;
        }
      }
      for (ConstructorDeclaration constructor : constructors) {
        BlockStmt block = constructor.getBody();
        Statement helper =
            HelperEffects.findLastWrite(block, VariableReferences::isScopeOnBoundComponent, change);
        if (findMatchingSetterCall(block, AstModifier::isMethodCallOnBoundComponent, change) != null
            || !initialized && !change.isRemoval()
                && !AstFinder.isDelegatingConstructor(constructor)) {
          applyBoundComponentSetter(block, change, helper);
        }
      }
      if (constructors.isEmpty() && !initialized && !change.isRemoval()) {
        ConstructorDeclaration constructor = classDecl.addConstructor();
        constructor.setBody(new BlockStmt());
        constructors = classDecl.getConstructors();
        applyBoundComponentSetter(constructor.getBody(), change, null);
      }
    }
  }

  private static void applyBoundComponentSetter(BlockStmt block, SourceChange sourceChange,
      Statement helper) {
    requireStableBinding(block, sourceChange.getItemDeclaration());
    requireStableAccessorBinding(block, sourceChange.getAccessor(),
        VariableReferences::isScopeOnBoundComponent, "getBoundComponent()");
    if (sourceChange.isRemoval()) {
      removeExistingCalls(block, AstModifier::isMethodCallOnBoundComponent, sourceChange);
      return;
    }
    Predicate<MethodCallExpr> scopeMatcher = AstModifier::isMethodCallOnBoundComponent;
    if (helper != null && !isWrittenAfter(block, scopeMatcher, sourceChange, helper)) {
      // The helper's write runs last, so the bound component's own write moves behind it.
      removeExistingCalls(block, scopeMatcher, sourceChange);
      Statement setterStmt = createBoundComponentSetterStatement(sourceChange);
      block.addStatement(VariableReferences.indexOfSame(block.getStatements(), helper) + 1,
          setterStmt);
      qualifyItemReference(setterStmt.asExpressionStmt().getExpression().asMethodCallExpr(),
          sourceChange);
      return;
    }
    if (sourceChange.getItemRef() != null) {
      if (!updateMatchingSetterCall(block, scopeMatcher, sourceChange)) {
        Statement setterStmt = createBoundComponentSetterStatement(sourceChange);
        insertItemCall(block, scopeMatcher, sourceChange, setterStmt);
      }
      return;
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

  private static boolean updateMatchingSetterCall(Node searchRoot,
      Predicate<MethodCallExpr> scopeMatcher, SourceChange sourceChange) {
    List<MethodCallExpr> matches = findMatchingSetterCalls(searchRoot, scopeMatcher, sourceChange);
    if (!matches.isEmpty()) {
      MethodCallExpr lastMatch = matches.get(matches.size() - 1);
      if (detachSharedItem(lastMatch, sourceChange)) {
        return false;
      }
      if (sourceChange.getItemRef() == null) {
        sourceChange.setReplacedComputedExpression(
            AstFinder.findComputedArguments(lastMatch, sourceChange.getMatchKey()));
      }
      lastMatch.getArguments().clear();
      for (Expression arg : sourceChange.getArguments()) {
        lastMatch.addArgument(arg);
      }
      qualifyItemReference(lastMatch, sourceChange);
      if (sourceChange.isReplaceAllCalls()) {
        for (int index = 0; index < matches.size() - 1; index++) {
          removeCallExpression(matches.get(index));
        }
      }

      return true;
    }

    return false;
  }


  private static MethodCallExpr findMatchingSetterCall(Node searchRoot,
      Predicate<MethodCallExpr> scopeMatcher, SourceChange sourceChange) {
    List<MethodCallExpr> matches = findMatchingSetterCalls(searchRoot, scopeMatcher, sourceChange);
    return matches.isEmpty() ? null : matches.get(matches.size() - 1);
  }

  private static List<MethodCallExpr> findMatchingSetterCalls(Node searchRoot,
      Predicate<MethodCallExpr> scopeMatcher, SourceChange sourceChange) {
    expandCombinedCalls(searchRoot, scopeMatcher, sourceChange);
    List<MethodCallExpr> matches = new ArrayList<>();
    String matchKey = sourceChange.getMatchKey();

    // A fluent receiver executes before the call wrapping it.
    for (MethodCallExpr methodCall : searchRoot.findAll(MethodCallExpr.class,
        Node.TreeTraversal.POSTORDER)) {
      if (methodCall.getNameAsString().equals(sourceChange.getMethodName())
          && scopeMatcher.test(methodCall) && isInExecutionScope(methodCall, searchRoot)
          && Objects.equals(getDirectAccessor(methodCall), sourceChange.getAccessor())) {
        if (matchKey != null && (methodCall.getArguments().isEmpty()
            || !methodCall.getArgument(0).toString().replace("\"", "").equals(matchKey))) {
          continue;
        }
        if (sourceChange.getItemRef() != null && !matchesItemCall(methodCall, sourceChange)) {
          continue;
        }
        if (searchRoot instanceof BlockStmt && hasConditionalExecution(methodCall, searchRoot)) {
          throw new SourceModificationException(sourceChange.getPropertyName()
              + " is set under a condition, so one value cannot replace it. "
              + "Edit the source directly.");
        }
        matches.add(methodCall);
      }
    }

    return matches;
  }

  private static void expandCombinedCalls(Node searchRoot, Predicate<MethodCallExpr> scopeMatcher,
      SourceChange change) {
    if (change.getMethodExpansions().isEmpty()) {
      return;
    }
    for (MethodCallExpr call : searchRoot.findAll(MethodCallExpr.class,
        Node.TreeTraversal.POSTORDER)) {
      List<String> setters = change.getMethodExpansions().get(call.getNameAsString());
      if (setters == null || !setters.contains(change.getMethodName()) || !scopeMatcher.test(call)
          || !isInExecutionScope(call, searchRoot)
          || !Objects.equals(getDirectAccessor(call), change.getAccessor())) {
        continue;
      }
      if (call.getArguments().size() != 1 && setters.size() != call.getArguments().size()
          || call.getScope().isEmpty()) {
        throw new SourceModificationException(
            "'" + call.getNameAsString() + "' has unexpected arguments, so "
                + change.getPropertyName() + " cannot be split out of it");
      }
      if (setters.size() > 1
          && call.getArguments().stream().anyMatch(argument -> !isConstantArgument(argument))) {
        throw new SourceModificationException(
            "'" + call.getNameAsString() + "' is called with computed arguments, so "
                + change.getPropertyName() + " cannot be split out of it");
      }
      if (searchRoot instanceof BlockStmt && hasConditionalExecution(call, searchRoot)) {
        throw new SourceModificationException(
            change.getPropertyName() + " is set under a condition, so one value cannot replace it. "
                + "Edit the source directly.");
      }
      Expression receiver = call.getScope().orElseThrow();
      List<Expression> arguments = call.getArguments().stream().map(Expression::clone).toList();
      call.removeScope();
      for (int index = 0; index < setters.size() - 1; index++) {
        receiver = new MethodCallExpr(receiver, setters.get(index))
            .addArgument(arguments.get(arguments.size() == 1 ? 0 : index).clone());
      }
      call.setScope(receiver);
      call.setName(setters.get(setters.size() - 1));
      call.setArguments(new NodeList<>(arguments.get(arguments.size() - 1)));
    }
  }

  private static boolean isConstantArgument(Expression argument) {
    if (argument.isLiteralExpr()) {
      return true;
    }
    if (argument.isEnclosedExpr()) {
      return isConstantArgument(argument.asEnclosedExpr().getInner());
    }
    if (argument.isUnaryExpr()) {
      return switch (argument.asUnaryExpr().getOperator()) {
        case PLUS, MINUS, LOGICAL_COMPLEMENT, BITWISE_COMPLEMENT ->
          isConstantArgument(argument.asUnaryExpr().getExpression());
        default -> false;
      };
    }
    return false;
  }


  private static void applyVariableSetters(CompilationUnit cu, BlockStmt block, String varName,
      List<SourceChange> sourceChanges, VariableDeclarator variable,
      Predicate<Expression> receiver) {
    Predicate<MethodCallExpr> scopeMatcher =
        variable == null ? call -> isMethodCallOnVariable(call, varName)
            : call -> VariableReferences.isCallOnVariable(call, variable);
    for (SourceChange sourceChange : sourceChanges) {
      requireStableBinding(block, sourceChange.getItemDeclaration());
      Statement helper =
          receiver == null ? null : HelperEffects.findLastWrite(block, receiver, sourceChange);
      if (sourceChange.isRemoval()) {
        removeExistingCalls(block, scopeMatcher, sourceChange);
        continue;
      }
      if (helper != null && !isWrittenAfter(block, scopeMatcher, sourceChange, helper)) {
        // The helper's write runs last, so the component's own write moves behind it.
        removeExistingCalls(block, scopeMatcher, sourceChange);
        Statement setterStmt = createSetterStatement(varName, sourceChange);
        block.addStatement(VariableReferences.indexOfSame(block.getStatements(), helper) + 1,
            setterStmt);
        qualifyItemReference(setterStmt.asExpressionStmt().getExpression().asMethodCallExpr(),
            sourceChange);
        if (variable != null) {
          VariableReferences.qualifyInsertedReceiver(setterStmt, variable);
        }
        continue;
      }
      if (sourceChange.getItemRef() != null) {
        if (!updateMatchingSetterCall(block, scopeMatcher, sourceChange)) {
          Statement setterStmt = createSetterStatement(varName, sourceChange);
          insertItemCall(block, scopeMatcher, sourceChange, setterStmt);
          if (variable != null) {
            VariableReferences.qualifyInsertedReceiver(setterStmt, variable);
          }
        }
        continue;
      }

      if (!updateMatchingSetterCall(block, scopeMatcher, sourceChange)) {
        Statement setterStmt = createSetterStatement(varName, sourceChange);
        int insertAfterIndex = findInsertionPointForVariable(block, varName);
        if (variable != null && insertAfterIndex >= 0) {
          Node declaration = variable;
          while (declaration.getParentNode().filter(parent -> parent != block).isPresent()) {
            declaration = declaration.getParentNode().orElseThrow();
          }
          insertAfterIndex = Math.max(insertAfterIndex, block.getStatements().indexOf(declaration));
        }
        if (insertAfterIndex >= 0) {
          block.addStatement(insertAfterIndex + 1, setterStmt);
        } else {
          block.addStatement(setterStmt);
        }
        if (variable != null) {
          VariableReferences.qualifyInsertedReceiver(setterStmt, variable);
        }
      }
    }
  }


  private static boolean isWrittenAfter(BlockStmt block, Predicate<MethodCallExpr> scopeMatcher,
      SourceChange change, Statement helper) {
    MethodCallExpr existing = findMatchingSetterCall(block, scopeMatcher, change);
    if (existing == null) {
      return false;
    }
    Node statement = existing;
    while (statement.getParentNode().orElse(null) != block) {
      statement = statement.getParentNode().orElseThrow();
    }

    return VariableReferences.indexOfSame(block.getStatements(), statement) > VariableReferences
        .indexOfSame(block.getStatements(), helper);
  }

  private static boolean detachSharedItem(MethodCallExpr call, SourceChange change) {
    if (change.getItemRef() == null || change.getItemPosition() != ItemPosition.LAST
        || call.getArguments().size() <= change.getItemArgumentCount()) {
      return false;
    }
    int valueCount = change.getItemArgumentCount() - 1;
    if (call.getArguments().stream().skip(valueCount)
        .allMatch(argument -> isItemReference(argument, change))) {
      return false;
    }
    detachItemArguments(call, change, valueCount);
    return true;
  }

  private static void qualifyItemReference(MethodCallExpr call, SourceChange change) {
    if (change.getItemDeclaration() != null) {
      int itemIndex =
          change.getItemPosition() == ItemPosition.FIRST ? 0 : change.getItemArgumentCount() - 1;
      VariableReferences.qualifyReference(call.getArgument(itemIndex), change.getItemDeclaration());
    }
  }

  private static void requireStableBinding(BlockStmt block, VariableDeclarator variable) {
    requireStableBinding(block, variable, null);
  }

  private static void requireStableBinding(BlockStmt block, VariableDeclarator variable,
      String accessor) {
    if (variable == null) {
      return;
    }
    if (variable.getParentNode().orElse(null) instanceof VariableDeclarationExpr) {
      BlockStmt declarationScope = variable.findAncestor(BlockStmt.class).orElse(null);
      if (declarationScope != block
          && (declarationScope == null || !declarationScope.isAncestorOf(block))) {
        throw new SourceModificationException("'" + variable.getNameAsString()
            + "' is declared in another block, so the write location cannot see it");
      }
    }
    for (AssignExpr assignment : block.findAll(AssignExpr.class)) {
      if (!isInExecutionScope(assignment, block)) {
        continue;
      }
      if (VariableReferences.isReferenceToVariable(assignment.getTarget(), variable)) {
        throw new SourceModificationException("'" + variable.getNameAsString()
            + "' is reassigned after it is created, so a write could reach a different component");
      }
    }
    requireStableAccessorBinding(block, accessor,
        expression -> VariableReferences.isScopeOnVariable(expression, variable),
        "'" + variable.getNameAsString() + "'");
  }

  private static void requireStableAccessorBinding(BlockStmt block, String accessor,
      Predicate<Expression> scopeMatcher, String owner) {
    if (accessor == null) {
      return;
    }
    for (AssignExpr assignment : block.findAll(AssignExpr.class)) {
      if (isInExecutionScope(assignment, block)
          && accessor.equals(VariableReferences.getAccessor(assignment.getTarget()))
          && scopeMatcher.test(assignment.getTarget())) {
        throw new SourceModificationException("The alias of '" + accessor + "' on " + owner
            + " is reassigned after it is created, so a write could reach a different component");
      }
    }
  }

  private static void removeExistingCalls(Node scope, Predicate<MethodCallExpr> scopeMatcher,
      SourceChange change) {
    MethodCallExpr call;
    while ((call = findMatchingSetterCall(scope, scopeMatcher, change)) != null) {
      if (!detachSharedItem(call, change)) {
        removeCallExpression(call);
      }
    }
  }

  static boolean isInExecutionScope(Node node, Node scope) {
    Node current = node;
    while (current != scope) {
      if (current instanceof LambdaExpr
          || current instanceof BodyDeclaration<?> && scope instanceof BlockStmt) {
        return false;
      }
      current = current.getParentNode().orElse(null);
      if (current == null) {
        return false;
      }
    }
    return true;
  }

  private static boolean hasConditionalExecution(Node node, Node scope) {
    Node current = node;
    while (current != scope && current != null) {
      if (current instanceof IfStmt || current instanceof SwitchEntry || current instanceof ForStmt
          || current instanceof ForEachStmt || current instanceof WhileStmt
          || current instanceof DoStmt || current instanceof ConditionalExpr
          || current instanceof CatchClause) {
        return true;
      }
      current = current.getParentNode().orElse(null);
    }
    return false;
  }

  private static void removeChainedCalls(Expression expression, SourceChange change) {
    Expression chain = expression;
    while (chain.getParentNode().orElse(null) instanceof MethodCallExpr call
        && call.getScope().orElse(null) == chain) {
      chain = call;
    }
    expandCombinedCalls(chain, call -> VariableReferences.isScopeOnExpression(call, expression),
        change);
    Expression current = expression;
    while (current.getParentNode().orElse(null) instanceof MethodCallExpr call
        && call.getScope().orElse(null) == current) {
      if (call.getNameAsString().equals(change.getMethodName())
          && Objects.equals(getDirectAccessor(call), change.getAccessor())) {
        current = removeCallExpression(call);
      } else {
        current = call;
      }
    }
  }

  private static Expression removeCallExpression(MethodCallExpr call) {
    Node parent = call.getParentNode().orElse(null);
    Expression receiver = call.getScope().orElse(null);
    if (parent instanceof ExpressionStmt statement && statement.getExpression() == call) {
      if (isPlainReceiver(receiver, getDirectAccessor(call))) {
        if (statement.remove() || statement.replace(new BlockStmt())) {
          return call;
        }
      } else if (receiver != null) {
        statement.setExpression(receiver);
        return receiver;
      }
    } else if (receiver != null) {
      if (call.replace(receiver)) {
        return receiver;
      }
    }
    throw new SourceModificationException("Removing '" + call.getNameAsString()
        + "' would change the statement around it, so it cannot be reset");
  }

  private static boolean isPlainReceiver(Expression expression, String accessor) {
    if (expression instanceof NameExpr || expression instanceof ThisExpr) {
      return true;
    }
    if (expression instanceof FieldAccessExpr field) {
      return isPlainReceiver(field.getScope(), null);
    }
    if (expression instanceof MethodCallExpr method && method.getArguments().isEmpty()) {
      if (method.getNameAsString().equals(GET_BOUND_COMPONENT)
          && method.getScope().map(ThisExpr.class::isInstance).orElse(true)) {
        return true;
      }
      return method.getNameAsString().equals(accessor)
          && method.getScope().map(scope -> isPlainReceiver(scope, null)).orElse(false);
    }
    return false;
  }
}
