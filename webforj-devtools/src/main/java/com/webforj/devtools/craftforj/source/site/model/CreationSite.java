package com.webforj.devtools.craftforj.source.site.model;

import java.util.Objects;

/**
 * The expression of a method that creates a component.
 *
 * <p>
 * A site names the method that holds the expression, what the expression is, and its position among
 * the expressions of the same kind and name inside that method. It is not found by its line, so it
 * keeps pointing at the same expression when the lines of the file move. The line only tells apart
 * what nothing else does.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class CreationSite {

  /**
   * What the expression of a site is.
   */
  public enum Kind {
    /** A {@code new Type(...)} expression. */
    CREATION,
    /** A method call. */
    CALL,
    /** A constructor that hands over to {@code super(...)} or {@code this(...)}. */
    DELEGATION,
    /** A method the compiler wrote to hand over to the one the source declares. */
    BRIDGE
  }

  private final String className;
  private final String methodName;
  private final String descriptor;
  private final Kind kind;
  private final String name;
  private final String producedType;
  private final int ordinal;
  private final int count;
  private final int line;

  private CreationSite(Builder builder) {
    this.className = builder.className;
    this.methodName = builder.methodName;
    this.descriptor = builder.descriptor;
    this.kind = builder.kind;
    this.name = builder.name;
    this.producedType = builder.producedType;
    this.ordinal = builder.ordinal;
    this.count = builder.count;
    this.line = builder.line;
  }

  /**
   * Creates a builder.
   *
   * @return the builder
   */
  public static Builder builder() {
    return new Builder();
  }

  /**
   * Gets the class that holds the expression.
   *
   * @return the binary class name, {@code com.example.View$Inner} for a nested class
   */
  public String getClassName() {
    return className;
  }

  /**
   * Gets the method that holds the expression.
   *
   * @return the method name, {@code <init>} for a constructor and {@code <clinit>} for the static
   *         initializer
   */
  public String getMethodName() {
    return methodName;
  }

  /**
   * Gets the descriptor of the method that holds the expression.
   *
   * @return the method descriptor
   */
  public String getDescriptor() {
    return descriptor;
  }

  /**
   * Gets what the expression is.
   *
   * @return the kind
   */
  public Kind getKind() {
    return kind;
  }

  /**
   * Gets the name the expression is counted under.
   *
   * @return the simple name of the created type, empty for an anonymous class, or the name of the
   *         called method
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the type the expression produces.
   *
   * @return the binary name of the created type or of the declared return type, or {@code null}
   *         when the expression produces no object
   */
  public String getProducedType() {
    return producedType;
  }

  /**
   * Gets the position of the expression among the expressions of the same kind and name.
   *
   * @return the zero based position in the order the method runs them
   */
  public int getOrdinal() {
    return ordinal;
  }

  /**
   * Gets how many expressions of the same kind and name the method holds.
   *
   * @return the number of expressions
   */
  public int getCount() {
    return count;
  }

  /**
   * Gets the line the compiler recorded for the expression.
   *
   * @return the line, or {@code 0} when it is not known
   */
  public int getLine() {
    return line;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }

    return other instanceof CreationSite site && ordinal == site.ordinal && count == site.count
        && line == site.line && kind == site.kind && Objects.equals(className, site.className)
        && Objects.equals(methodName, site.methodName)
        && Objects.equals(descriptor, site.descriptor) && Objects.equals(name, site.name)
        && Objects.equals(producedType, site.producedType);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    return Objects.hash(className, methodName, descriptor, kind, name, producedType, ordinal, count,
        line);
  }

  /**
   * Builds a {@link CreationSite}.
   */
  public static final class Builder {

    private String className;
    private String methodName;
    private String descriptor;
    private Kind kind;
    private String name;
    private String producedType;
    private int ordinal;
    private int count;
    private int line;

    private Builder() {}

    /**
     * Sets the class that holds the expression.
     *
     * @param className the binary class name
     * @return this builder
     */
    public Builder setClassName(String className) {
      this.className = className;

      return this;
    }

    /**
     * Sets the method that holds the expression.
     *
     * @param methodName the method name
     * @param descriptor the method descriptor
     * @return this builder
     */
    public Builder setMethod(String methodName, String descriptor) {
      this.methodName = methodName;
      this.descriptor = descriptor;

      return this;
    }

    /**
     * Sets what the expression is.
     *
     * @param kind the kind
     * @return this builder
     */
    public Builder setKind(Kind kind) {
      this.kind = kind;

      return this;
    }

    /**
     * Sets the name the expression is counted under.
     *
     * @param name the simple name of the created type or the name of the called method
     * @return this builder
     */
    public Builder setName(String name) {
      this.name = name;

      return this;
    }

    /**
     * Sets the type the expression produces.
     *
     * @param producedType the binary type name, or {@code null} when no object is produced
     * @return this builder
     */
    public Builder setProducedType(String producedType) {
      this.producedType = producedType;

      return this;
    }

    /**
     * Sets the position of the expression among the expressions of the same kind and name.
     *
     * @param ordinal the zero based position
     * @param count the number of expressions
     * @return this builder
     */
    public Builder setPosition(int ordinal, int count) {
      this.ordinal = ordinal;
      this.count = count;

      return this;
    }

    /**
     * Sets the line the compiler recorded for the expression.
     *
     * @param line the line
     * @return this builder
     */
    public Builder setLine(int line) {
      this.line = line;

      return this;
    }

    /**
     * Builds the site.
     *
     * @return the site
     */
    public CreationSite build() {
      return new CreationSite(this);
    }
  }
}
