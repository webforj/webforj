package com.webforj.devtools.craftforj.source.model;

import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Represents a source code change to be applied.
 *
 * <p>
 * This class encapsulates what method to call and with what arguments. It also tracks any imports
 * that need to be added to the source file (e.g., for enum types).
 * </p>
 *
 * <p>
 * Use the builder to create instances:
 * </p>
 *
 * <pre>
 * SourceChange change =
 *     SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello"))
 *         .addImport("com.webforj.component.button.ButtonTheme").build();
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceChange {

  /**
   * Position of the item reference argument in a parent-scoped item call.
   */
  public enum ItemPosition {
    /** The item is the first argument, e.g. {@code setSpan(item, 2)}. */
    FIRST,
    /** The item is the trailing (possibly varargs) argument, e.g. {@code setItemGrow(1, item)}. */
    LAST
  }

  private final List<String> imports;
  private final String methodName;
  private final List<Expression> arguments;
  private final String matchKey;
  private final String accessor;
  private final String itemRef;
  private final ItemPosition itemPosition;
  private final int itemArgumentCount;
  private final boolean removal;
  private final boolean replaceAllCalls;
  private final Map<String, List<String>> methodExpansions;
  private final String propertyName;
  private VariableDeclarator itemDeclaration;
  private String replacedComputedExpression;

  private SourceChange(Builder builder) {
    this.imports = Collections.unmodifiableList(new ArrayList<>(builder.imports));
    this.methodName = builder.methodName;
    this.arguments = Collections.unmodifiableList(new ArrayList<>(builder.arguments));
    this.matchKey = builder.matchKey;
    this.accessor = builder.accessor;
    this.itemRef = builder.itemRef;
    this.itemPosition = builder.itemPosition;
    this.itemArgumentCount =
        builder.itemArgumentCount < 0 ? builder.arguments.size() : builder.itemArgumentCount;
    this.removal = builder.removal;
    this.replaceAllCalls = builder.replaceAllCalls;
    this.methodExpansions = Map.copyOf(builder.methodExpansions);
    this.propertyName = builder.propertyName;
  }

  /**
   * Gets the imports that need to be added.
   *
   * @return unmodifiable list of fully qualified class names to import
   */
  public List<String> getImports() {
    return imports;
  }

  /**
   * Gets the method name to call.
   *
   * @return the setter method name (e.g., "setText", "setTheme")
   */
  public String getMethodName() {
    return methodName;
  }

  /**
   * Indicates that existing calls should be removed from the selected component's scope.
   *
   * @return true for a removal
   */
  public boolean isRemoval() {
    return removal;
  }

  /**
   * Indicates that the supplied values replace all accumulating calls in the selected scope.
   *
   * @return true when earlier matching calls must be removed
   */
  public boolean isReplaceAllCalls() {
    return replaceAllCalls;
  }

  /**
   * Gets the argument expressions for the method call.
   *
   * @return unmodifiable list of JavaParser expressions for the arguments
   */
  public List<Expression> getArguments() {
    return arguments;
  }

  /**
   * Gets the first argument expression (convenience for single-arg methods).
   *
   * @return the first JavaParser expression, or null if no arguments
   */
  public Expression getArgument() {
    return arguments.isEmpty() ? null : arguments.get(0);
  }

  /**
   * Gets the name the inspector shows for the property, used in messages to the developer.
   *
   * @return the property name, or the setter name when no property name was attached
   */
  public String getPropertyName() {
    return propertyName != null ? propertyName : methodName;
  }

  /**
   * Gets the key for matching existing calls.
   *
   * @return the match key, or null if matching by method name only
   */
  public String getMatchKey() {
    return matchKey;
  }

  /**
   * Gets the accessor method that scopes the setter call.
   *
   * @return the accessor method name (e.g., "getSearch"), or null for direct setter calls
   */
  public String getAccessor() {
    return accessor;
  }

  /**
   * Gets the variable name of the item referenced by a parent-scoped item call.
   *
   * <p>
   * For writes, the arguments contain a {@code NameExpr} for this variable at the position
   * indicated by {@link #getItemPosition()}. The item reference drives matching of existing calls:
   * only calls that reference the same item variable are updated or removed.
   * </p>
   *
   * @return the item variable name, or null for regular setter calls
   */
  public String getItemRef() {
    return itemRef;
  }

  /**
   * Gets the position of the item reference argument.
   *
   * @return the item position, or null when {@link #getItemRef()} is null
   */
  public ItemPosition getItemPosition() {
    return itemPosition;
  }

  /**
   * Gets the argument count of a single-item call, including value arguments.
   *
   * @return the argument count used to distinguish item overloads, including for removals
   */
  public int getItemArgumentCount() {
    return itemArgumentCount;
  }

  /**
   * Gets the selected child declaration in the compilation unit being edited.
   *
   * @return the bound declaration, or null for legacy name-only item matching
   */
  public VariableDeclarator getItemDeclaration() {
    return itemDeclaration;
  }

  /**
   * Binds item matching and inserted references to a declaration in this edit's compilation unit.
   *
   * @param itemDeclaration the selected child declaration
   */
  public void setItemDeclaration(VariableDeclarator itemDeclaration) {
    this.itemDeclaration = itemDeclaration;
  }

  /**
   * Gets the computed expression the applied update overwrote.
   *
   * <p>
   * Feedback from the apply step. Set when the updated call's previous argument computed its value
   * instead of holding a literal, so the caller can surface that the written literal erases logic.
   * </p>
   *
   * @return the overwritten expression as source text, or null when nothing computed was replaced
   */
  public String getReplacedComputedExpression() {
    return replacedComputedExpression;
  }

  /**
   * Sets the computed expression the applied update overwrote.
   *
   * @param replacedComputedExpression the overwritten expression as source text
   */
  public void setReplacedComputedExpression(String replacedComputedExpression) {
    this.replacedComputedExpression = replacedComputedExpression;
  }

  /**
   * Creates a copy of this change with the given accessor.
   *
   * @param accessor the accessor method name (e.g., "getSearch")
   * @return a new SourceChange scoped by the accessor
   */
  public SourceChange withAccessor(String accessor) {
    Builder builder = copyBuilder().accessor(accessor);
    SourceChange change = builder.build();
    change.itemDeclaration = itemDeclaration;
    return change;
  }

  /**
   * Gets the ordered argument setters of equivalent combined calls.
   *
   * @return immutable method expansions supplied by the property handler
   */
  public Map<String, List<String>> getMethodExpansions() {
    return methodExpansions;
  }

  /**
   * Attaches the property name the inspector shows for this change.
   *
   * @param propertyName the property name
   * @return a copy carrying the property name
   */
  public SourceChange withPropertyName(String propertyName) {
    Builder builder = copyBuilder().propertyName(propertyName);
    SourceChange change = builder.build();
    change.itemDeclaration = itemDeclaration;
    return change;
  }

  /**
   * Attaches combined-call semantics without changing the requested operation.
   *
   * @param expansions combined methods mapped to one setter per argument
   * @return a copy carrying the supplied expansions
   */
  public SourceChange withMethodExpansions(Map<String, List<String>> expansions) {
    Builder builder = copyBuilder();
    builder.methodExpansions = expansions.entrySet().stream().collect(java.util.stream.Collectors
        .toUnmodifiableMap(Map.Entry::getKey, entry -> List.copyOf(entry.getValue())));
    SourceChange change = builder.build();
    change.itemDeclaration = itemDeclaration;
    return change;
  }

  private Builder copyBuilder() {
    Builder builder = removal ? builder().removeMethodCall(methodName)
        : builder().methodCall(methodName, arguments);
    builder.matchKey(matchKey).accessor(accessor).itemRef(itemRef, itemPosition, itemArgumentCount)
        .replaceAllCalls(replaceAllCalls).propertyName(propertyName);
    imports.forEach(builder::addImport);
    builder.methodExpansions = methodExpansions;
    return builder;
  }

  /**
   * Creates a new builder.
   *
   * @return a new SourceChange builder
   */
  public static Builder builder() {
    return new Builder();
  }

  /**
   * Builder for SourceChange.
   */
  public static class Builder {

    private final List<String> imports = new ArrayList<>();
    private final List<Expression> arguments = new ArrayList<>();
    private String methodName;
    private String matchKey;
    private String accessor;
    private String itemRef;
    private ItemPosition itemPosition;
    private int itemArgumentCount = -1;
    private boolean removal;
    private boolean replaceAllCalls;
    private String propertyName;
    private Map<String, List<String>> methodExpansions = Map.of();

    Builder() {}

    /**
     * Sets whether earlier accumulating calls must be removed.
     *
     * @param replaceAllCalls true to replace the complete accumulated value
     * @return this builder
     */
    public Builder replaceAllCalls(boolean replaceAllCalls) {
      this.replaceAllCalls = replaceAllCalls;
      return this;
    }

    /**
     * Sets the name the inspector shows for the property.
     *
     * @param propertyName the property name
     * @return this builder
     */
    public Builder propertyName(String propertyName) {
      this.propertyName = propertyName;
      return this;
    }

    /**
     * Adds an import to include in the source file.
     *
     * @param qualifiedName the fully qualified class name (e.g.,
     *        "com.webforj.component.ButtonTheme")
     * @return this builder
     */
    public Builder addImport(String qualifiedName) {
      if (qualifiedName != null && !qualifiedName.isBlank()) {
        imports.add(qualifiedName);
      }

      return this;
    }

    /**
     * Sets the method call with multiple arguments (varargs).
     *
     * @param methodName the method name
     * @param arguments the expressions for the arguments
     * @return this builder
     */
    public Builder methodCall(String methodName, List<Expression> arguments) {
      this.methodName = methodName;
      this.removal = false;
      this.arguments.clear();
      this.arguments.addAll(arguments);

      return this;
    }

    /**
     * Sets the method call with a single argument.
     *
     * @param methodName the setter method name
     * @param argument the expression for the argument
     * @return this builder
     */
    public Builder methodCall(String methodName, Expression argument) {
      return methodCall(methodName, List.of(argument));
    }

    /**
     * Removes existing calls instead of inserting or replacing arguments.
     *
     * @param methodName the method name to remove
     * @return this builder
     */
    public Builder removeMethodCall(String methodName) {
      this.methodName = methodName;
      this.removal = true;
      this.arguments.clear();
      return this;
    }

    /**
     * Sets the key for matching existing calls.
     *
     * @param key the key to match
     * @return this builder
     */
    public Builder matchKey(String key) {
      this.matchKey = key;
      return this;
    }

    /**
     * Sets the accessor method that scopes the setter call.
     *
     * <p>
     * When set, the generated call is {@code variable.accessor().setter(value)} instead of
     * {@code variable.setter(value)}.
     * </p>
     *
     * @param accessor the accessor method name (e.g., "getSearch")
     * @return this builder
     */
    public Builder accessor(String accessor) {
      this.accessor = accessor;
      return this;
    }

    /**
     * Marks this change as a parent-scoped item call referencing the given item variable.
     *
     * <p>
     * The arguments must already contain a {@code NameExpr} for the item variable at the given
     * position. Both values may be null to keep the change a regular setter call.
     * </p>
     *
     * @param itemRef the item variable name
     * @param itemPosition the position of the item argument
     * @return this builder
     */
    public Builder itemRef(String itemRef, ItemPosition itemPosition) {
      return itemRef(itemRef, itemPosition, -1);
    }

    /**
     * Marks an item call with an explicit arity so removals can match without value arguments.
     *
     * @param itemRef the item variable name
     * @param itemPosition the position of the item argument
     * @param itemArgumentCount the single-item argument count, or -1 to derive it from arguments
     * @return this builder
     */
    public Builder itemRef(String itemRef, ItemPosition itemPosition, int itemArgumentCount) {
      this.itemRef = itemRef;
      this.itemPosition = itemPosition;
      this.itemArgumentCount = itemArgumentCount;

      return this;
    }

    /**
     * Builds the SourceChange.
     *
     * @return the built SourceChange
     * @throws IllegalStateException if methodName is not set or a write has no arguments
     */
    public SourceChange build() {
      if (methodName == null || methodName.isBlank()) {
        throw new IllegalStateException("Method name is required");
      }

      if (!removal && arguments.isEmpty()) {
        throw new IllegalStateException("At least one argument is required");
      }

      return new SourceChange(this);
    }
  }
}
