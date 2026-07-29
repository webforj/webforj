package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.Expression;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
  private String replacedComputedExpression;

  private SourceChange(Builder builder) {
    this.imports = Collections.unmodifiableList(new ArrayList<>(builder.imports));
    this.methodName = builder.methodName;
    this.arguments = Collections.unmodifiableList(new ArrayList<>(builder.arguments));
    this.matchKey = builder.matchKey;
    this.accessor = builder.accessor;
    this.itemRef = builder.itemRef;
    this.itemPosition = builder.itemPosition;
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
   * When set, the arguments already contain a {@code NameExpr} for this variable at the position
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
    Builder builder = builder().methodCall(methodName, arguments).matchKey(matchKey)
        .accessor(accessor).itemRef(itemRef, itemPosition);
    imports.forEach(builder::addImport);

    return builder.build();
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

    Builder() {}

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
      this.itemRef = itemRef;
      this.itemPosition = itemPosition;

      return this;
    }

    /**
     * Builds the SourceChange.
     *
     * @return the built SourceChange
     * @throws IllegalStateException if methodName is not set or arguments is empty
     */
    public SourceChange build() {
      if (methodName == null || methodName.isBlank()) {
        throw new IllegalStateException("Method name is required");
      }

      if (arguments.isEmpty()) {
        throw new IllegalStateException("At least one argument is required");
      }

      return new SourceChange(this);
    }
  }
}
