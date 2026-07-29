package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator;
import java.util.Optional;

/**
 * Interface for handling a specific feature type.
 *
 * <p>
 * Each concern (HasText, HasTheme, HasWidth, etc.) should have its own contribution. Extend
 * {@link ConcernContribution} for simple properties, {@link EnumConcernContribution} for enum-based
 * properties, or {@link ListConcernContribution} for list properties.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface FeatureHandler {

  /**
   * Gets the feature interface class this handler manages.
   *
   * @return the feature interface class
   */
  Class<?> getFeatureInterface();

  /**
   * Gets the unique identifier for this feature type.
   *
   * <p>
   * Defaults to the contribution class name with "Contribution" suffix removed. For example,
   * {@code HasTextContribution} returns "HasText", {@code SliderFilledContribution} returns
   * "SliderFilled".
   * </p>
   *
   * @return the feature type identifier (e.g., "HasText", "SliderFilled")
   */
  default String getFeatureType() {
    return getClass().getSimpleName().replace("Contribution", "");
  }

  /**
   * Checks if the given component supports this feature.
   *
   * <p>
   * For Composite components, also checks if the bound component implements the feature.
   * </p>
   *
   * @param component the component to check
   * @return true if the component or its bound component implements this feature
   */
  default boolean supports(Component component) {
    return supports(component, true);
  }

  /**
   * Checks if the given component supports this feature.
   *
   * @param component the component to check
   * @param allowBoundFallback if true and component is a Composite, checks bound component too
   * @return true if the component supports this feature
   */
  default boolean supports(Component component, boolean allowBoundFallback) {
    return getTargetComponent(component, allowBoundFallback) != null;
  }

  /**
   * Gets the component that implements the feature interface.
   *
   * <p>
   * For regular components, returns the component itself if it implements the feature. For
   * Composite components, also checks if the bound component implements the feature.
   * </p>
   *
   * @param component the component to check
   * @return the target component that implements the feature, or null if none
   */
  default Component getTargetComponent(Component component) {
    return getTargetComponent(component, true);
  }

  /**
   * Gets the component that implements the feature interface.
   *
   * <p>
   * For regular components, returns the component itself if it implements the feature. For
   * Composite components, falls back to bound component only if {@code allowBoundFallback} is true.
   * </p>
   *
   * <p>
   * The {@code allowBoundFallback} parameter controls whether to check the bound component of a
   * Composite. Set to {@code true} when inspecting the root component (the class being edited),
   * where we can generate code like {@code getBoundComponent().setX()}. Set to {@code false} when
   * inspecting nested components (used inside another class), where we can only call methods on the
   * component's public API.
   * </p>
   *
   * @param component the component to check
   * @param allowBoundFallback if true and component is a Composite, checks bound component too
   * @return the target component that implements the feature, or null if none
   */
  default Component getTargetComponent(Component component, boolean allowBoundFallback) {
    Class<?> featureInterface = getFeatureInterface();
    if (featureInterface.isInstance(component)) {
      return component;
    }
    if (allowBoundFallback && component instanceof Composite) {
      Component bound = ComponentUtil.getBoundComponent(component);
      if (featureInterface.isInstance(bound)) {
        return bound;
      }
    }

    return null;
  }

  /**
   * Extracts the feature property from the component.
   *
   * @param component the component to extract from
   * @return an Optional containing the property if extraction succeeded, empty otherwise
   */
  Optional<FeatureProperty> get(Component component);

  /**
   * Applies a value to the component for this feature.
   *
   * @param component the component to modify
   * @param value the value to apply
   * @return true if the value was successfully applied
   */
  boolean set(Component component, Object value);

  /**
   * Applies a value to the component for this feature, with the parent component available.
   *
   * <p>
   * Parent-scoped handlers (see {@link #isParentScoped()}) override this variant because the value
   * is applied through the parent's API, e.g. {@code flexLayout.setItemGrow(1, item)}. The default
   * ignores the parent and delegates to {@link #set(Component, Object)}.
   * </p>
   *
   * @param component the component to modify
   * @param parent the parent component as resolved by the client tree, or null when unknown
   * @param value the value to apply
   * @return true if the value was successfully applied
   */
  default boolean set(Component component, Component parent, Object value) {
    return set(component, value);
  }

  /**
   * Indicates whether this feature is applied through the parent component's API.
   *
   * <p>
   * Parent-scoped features generate source code on the parent's variable with the component passed
   * as an argument ({@code flexLayout.setItemGrow(1, item)}) instead of a setter on the component
   * itself. The client must send the parent component along with changes for such features.
   * </p>
   *
   * @return true if this feature is parent-scoped (default: false)
   */
  default boolean isParentScoped() {
    return false;
  }

  /**
   * Gets the category this feature belongs to.
   *
   * @return the feature category
   */
  FeatureCategory getCategory();

  /**
   * Checks if this feature should appear based on parent component type.
   *
   * <p>
   * Override this method to create contributions that only appear when the component is a child of
   * a specific parent type. For example, flex item properties only appear when the parent is a
   * FlexLayout.
   * </p>
   *
   * @param parentType the fully qualified class name of the parent component, or null if no parent
   * @return true if this feature should appear (default: true)
   */
  default boolean supportsParent(String parentType) {
    return true;
  }

  /**
   * Gets the method name for source code generation.
   *
   * <p>
   * Default is "set" + propertyName. Override for handlers that use different naming (e.g.,
   * "addClassName" for list properties).
   * </p>
   *
   * @param propertyName the property name
   * @return the method name to use in generated source code
   */
  default String getSourceMethodName(String propertyName) {
    return "set" + propertyName;
  }

  /**
   * Gets a custom source generator for this feature.
   *
   * <p>
   * Most handlers rely on the generator selected from their base class (scalar, enum, key-value,
   * list). Handlers whose values need a custom expression shape (e.g. a list of constructor calls)
   * return their own generator here.
   * </p>
   *
   * @return the custom source generator, or null to use the default selection
   */
  default SourceGenerator getSourceGenerator() {
    return null;
  }

  /**
   * Gets the accessor method that scopes the setter in source code generation.
   *
   * <p>
   * Default is null, which generates {@code component.setX(value)}. Override for handlers that
   * write to a nested configuration object. For example, returning "getSearch" generates
   * {@code component.getSearch().setX(value)}.
   * </p>
   *
   * @return the accessor method name, or null for direct setter calls
   */
  default String getSourceAccessor() {
    return null;
  }

  /**
   * Transforms the value for source code generation.
   *
   * <p>
   * Most handlers return the value unchanged. Override for handlers that need to restructure the
   * value before it reaches the source generator. For example, key-value handlers (like style
   * properties) wrap the value with the key: {@code "center"} becomes {@code ["align-self",
   * "center"]} so the generator can produce {@code setStyle("align-self", "center")}.
   * </p>
   *
   * <p>
   * The property parameter provides access to {@code javaType} for proper value formatting.
   * </p>
   *
   * @param property the property containing value and type metadata
   * @return the value structure expected by the source generator
   */
  default Object getSourceValue(FeatureProperty property) {
    return property.getValue();
  }

  /**
   * Gets the base name of an optional {@link java.util.ResourceBundle} contributing property
   * description translations for this handler's feature type.
   *
   * <p>
   * When provided, the bundle is expected to contribute keys of the form
   * {@code props.<FeatureType>.desc} for this handler's locale catalog. Override this method to
   * ship descriptions alongside a custom contribution.
   * </p>
   *
   * @return the resource bundle base name, or null when this handler contributes no translations
   */
  default String getTranslationBundle() {
    return null;
  }

}
