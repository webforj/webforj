package com.webforj.devtools.craftforj.inspector.model;

import com.google.gson.annotations.JsonAdapter;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a single property within a feature group.
 *
 * <p>
 * This class is used by the craftforJ inspector to display and edit component properties. Each
 * property has a name, feature type, editor type for rendering, editor configuration, Java type for
 * source generation, and current value.
 * </p>
 *
 * <p>
 * The property travels round-trip between server and client: the server builds the full property
 * with all metadata, the client updates only the value, and sends the complete property back for
 * source generation.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FeatureProperty {

  private final String name;
  private final String featureType;
  private final PropertyType editorType;
  private final Map<String, Object> editorConfig;
  @JsonAdapter(ClassTypeAdapter.class)
  private final Class<?> javaType;
  private final Object value;
  private final boolean parentScoped;

  private FeatureProperty(Builder builder) {
    this.name = builder.name;
    this.featureType = builder.featureType;
    this.editorType = builder.editorType;
    this.editorConfig = builder.editorConfig;
    this.javaType = builder.javaType;
    this.value = builder.value;
    this.parentScoped = builder.parentScoped;
  }

  /**
   * Gets the property name.
   *
   * @return the property name (e.g., "Text", "MaxRowCount")
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the feature type this property belongs to.
   *
   * @return the feature type (e.g., "HasText", "HasMaxRowCount")
   */
  public String getFeatureType() {
    return featureType;
  }

  /**
   * Gets the editor type for UI rendering.
   *
   * @return the editor type
   */
  public PropertyType getEditorType() {
    return editorType;
  }

  /**
   * Gets the editor configuration.
   *
   * @return the editor config map, or null if not applicable
   */
  public Map<String, Object> getEditorConfig() {
    return editorConfig;
  }

  /**
   * Gets the Java type for source code generation.
   *
   * @return the Java class type (e.g., Integer.class, String.class)
   */
  public Class<?> getJavaType() {
    return javaType != null ? javaType : String.class;
  }

  /**
   * Gets the current value of the property.
   *
   * @return the current value
   */
  public Object getValue() {
    return value;
  }

  /**
   * Checks whether this property is applied through the parent component's API.
   *
   * <p>
   * The client must send the parent component along with changes for parent-scoped properties, so
   * both live changes and source generation can go through the parent.
   * </p>
   *
   * @return true if the property is parent-scoped
   */
  public boolean isParentScoped() {
    return parentScoped;
  }

  /**
   * Creates a new builder for FeatureProperty.
   *
   * @param name the property name
   * @param featureType the feature type
   * @return a new builder instance
   */
  public static Builder builder(String name, String featureType) {
    return new Builder(name, featureType);
  }

  /**
   * Builder for FeatureProperty with type methods that couple editorType, editorConfig, and
   * javaType together.
   */
  public static class Builder {

    private final String name;
    private final String featureType;
    private PropertyType editorType = PropertyType.TEXT;
    private Map<String, Object> editorConfig;
    private Class<?> javaType = String.class;
    private Object value;
    private boolean parentScoped;

    Builder(String name, String featureType) {
      this.name = name;
      this.featureType = featureType;
    }

    /**
     * Configures for text input.
     *
     * @return this builder
     */
    public Builder text() {
      this.editorType = PropertyType.TEXT;
      this.editorConfig = null;
      this.javaType = String.class;

      return this;
    }

    /**
     * Configures for text input with configuration.
     *
     * @param config the editor config (pattern, maxLength, minLength, placeholder)
     * @return this builder
     */
    public Builder text(Map<String, Object> config) {
      this.editorType = PropertyType.TEXT;
      this.editorConfig = config;
      this.javaType = String.class;

      return this;
    }

    /**
     * Configures for integer input.
     *
     * @return this builder
     */
    public Builder integer() {
      this.editorType = PropertyType.NUMBER;
      this.editorConfig = Map.of("step", 1);
      this.javaType = Integer.class;

      return this;
    }

    /**
     * Configures for integer input with min/max bounds.
     *
     * @param min the minimum value
     * @param max the maximum value
     * @return this builder
     */
    public Builder integer(int min, int max) {
      this.editorType = PropertyType.NUMBER;
      this.editorConfig = Map.of("step", 1, "min", min, "max", max);
      this.javaType = Integer.class;

      return this;
    }

    /**
     * Configures for decimal input.
     *
     * @return this builder
     */
    public Builder decimal() {
      this.editorType = PropertyType.NUMBER;
      this.editorConfig = Map.of("step", "any");
      this.javaType = Double.class;

      return this;
    }

    /**
     * Configures for decimal input with step.
     *
     * @param step the step value
     * @return this builder
     */
    public Builder decimal(double step) {
      this.editorType = PropertyType.NUMBER;
      this.editorConfig = Map.of("step", step);
      this.javaType = Double.class;

      return this;
    }

    /**
     * Configures for decimal input with step and bounds.
     *
     * @param step the step value
     * @param min the minimum value
     * @param max the maximum value
     * @return this builder
     */
    public Builder decimal(double step, double min, double max) {
      this.editorType = PropertyType.NUMBER;
      this.editorConfig = Map.of("step", step, "min", min, "max", max);
      this.javaType = Double.class;

      return this;
    }

    /**
     * Configures for boolean checkbox.
     *
     * @return this builder
     */
    public Builder bool() {
      this.editorType = PropertyType.BOOLEAN;
      this.editorConfig = null;
      this.javaType = Boolean.class;

      return this;
    }

    /**
     * Configures for select dropdown with options.
     *
     * @param options list of select options
     * @return this builder
     */
    public Builder select(List<SelectOption> options) {
      this.editorType = PropertyType.SELECT;
      this.editorConfig = Map.of("options", options);
      this.javaType = String.class;

      return this;
    }

    /**
     * Configures for enum select dropdown.
     *
     * @param <E> the enum type
     * @param enumClass the enum class
     * @return this builder
     */
    public <E extends Enum<E>> Builder enumOf(Class<E> enumClass) {
      this.editorType = PropertyType.SELECT;
      this.editorConfig = Map.of("options", buildEnumOptions(enumClass));
      this.javaType = enumClass;

      return this;
    }

    /**
     * Configures for list editor with string items.
     *
     * @return this builder
     */
    public Builder list() {
      return listOf(String.class);
    }

    /**
     * Configures for list editor with typed items.
     *
     * @param itemClass the item class type
     * @return this builder
     */
    public Builder listOf(Class<?> itemClass) {
      this.editorType = PropertyType.LIST;
      this.editorConfig = Map.of("itemType", itemClass.getSimpleName().toLowerCase());
      this.javaType = List.class;

      return this;
    }

    /**
     * Configures for icon picker.
     *
     * <p>
     * The value is a string combining the pool and the icon name separated by a colon, e.g.
     * {@code "tabler:home"}.
     * </p>
     *
     * @return this builder
     */
    public Builder icon() {
      this.editorType = PropertyType.ICON;
      this.editorConfig = null;
      this.javaType = String.class;

      return this;
    }

    /**
     * Configures for size editor (CSS dimensions).
     *
     * @return this builder
     */
    public Builder size() {
      this.editorType = PropertyType.SIZE;
      this.editorConfig = null;
      this.javaType = String.class;

      return this;
    }

    /**
     * Sets the Java type for source generation.
     *
     * @param type the Java class type
     * @return this builder
     */
    public Builder javaType(Class<?> type) {
      this.javaType = type;
      return this;
    }

    /**
     * Sets the current value.
     *
     * @param value the value
     * @return this builder
     */
    public Builder value(Object value) {
      this.value = value;
      return this;
    }

    /**
     * Marks the property as applied through the parent component's API.
     *
     * @param parentScoped true when the property is parent-scoped
     * @return this builder
     */
    public Builder parentScoped(boolean parentScoped) {
      this.parentScoped = parentScoped;
      return this;
    }

    /**
     * Hides the property from the generic editor rows.
     *
     * <p>
     * Hidden properties are edited exclusively through a dedicated visual editor (e.g. the columns
     * layout breakpoints table) while still traveling through the normal property pipeline.
     * </p>
     *
     * @return this builder
     */
    public Builder hidden() {
      Map<String, Object> config = this.editorConfig == null ? new LinkedHashMap<>()
          : new LinkedHashMap<>(this.editorConfig);
      config.put("hidden", true);
      this.editorConfig = config;

      return this;
    }

    /**
     * Builds the FeatureProperty.
     *
     * @return the built FeatureProperty
     */
    public FeatureProperty build() {
      return new FeatureProperty(this);
    }

    private <E extends Enum<E>> List<SelectOption> buildEnumOptions(Class<E> enumClass) {
      return Arrays.stream(enumClass.getEnumConstants())
          .map(e -> new SelectOption(e.name(), e.name())).toList();
    }
  }
}
