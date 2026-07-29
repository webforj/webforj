package com.webforj.devtools.craftforj.inspector.source.generator;

import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contract for source code generators.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 *
 * @see ScalarSourceGenerator
 * @see EnumSourceGenerator
 * @see ListSourceGenerator
 */
@FunctionalInterface
public interface SourceGenerator {

  /**
   * Generates a source change.
   *
   * @param context the generator context
   * @return the source change
   */
  SourceChange generate(GeneratorContext context);

  /**
   * Context for source generation.
   *
   * <p>
   * Contains the method name to call and the full FeatureProperty with value and type metadata. The
   * property's javaType is used by generators to format literals correctly (e.g., Integer vs
   * Double).
   * </p>
   */
  class GeneratorContext {

    private final String methodName;
    private final FeatureProperty property;

    /**
     * Creates a generator context.
     *
     * @param methodName the method name to call (e.g., "setText", "addClassName")
     * @param property the full property including value and javaType for source generation
     */
    public GeneratorContext(String methodName, FeatureProperty property) {
      this.methodName = methodName;
      this.property = property;
    }

    /**
     * Gets the method name to call.
     *
     * @return the method name
     */
    public String getMethodName() {
      return methodName;
    }

    /**
     * Gets the value from the property.
     *
     * @return the value to generate
     */
    public Object getValue() {
      return property != null ? property.getValue() : null;
    }

    /**
     * Gets the Java type for source generation.
     *
     * @return the expected Java type
     */
    public Class<?> getJavaType() {
      return property != null ? property.getJavaType() : null;
    }
  }
}
