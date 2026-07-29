package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Base class for key-value style contributions like setStyle(key, value).
 *
 * <p>
 * Handles properties where the method takes two arguments: a key and a value. For source
 * generation, the value is stored as a List [key, value] so the generator can produce:
 * {@code component.setStyle("flex-grow", "1")}
 * </p>
 *
 * <p>
 * The key can be fixed (defined in the contribution) or dynamic (sent from client).
 * </p>
 *
 * @param <T> the concern interface type
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class KeyValueConcernContribution<T> implements FeatureHandler {

  private static final Logger LOGGER =
      Logger.getLogger(KeyValueConcernContribution.class.getName());

  private final Class<?> concernInterface;
  private final String key;
  private final String displayName;
  private final FeatureCategory category;
  private UnaryOperator<FeatureProperty.Builder> builderConfig = FeatureProperty.Builder::text;
  private Function<T, Object> getter;
  private BiConsumer<T, Object> setter;

  /**
   * Creates a new key-value contribution with a fixed key.
   *
   * @param concernInterface the concern interface class
   * @param key the fixed key (e.g., CSS property name like "flex-grow")
   * @param displayName the display name shown in UI (e.g., "Grow")
   * @param category the feature category
   */
  protected KeyValueConcernContribution(Class<?> concernInterface, String key, String displayName,
      FeatureCategory category) {
    this.concernInterface = concernInterface;
    this.key = key;
    this.displayName = displayName;
    this.category = category;
  }

  @Override
  public Class<?> getFeatureInterface() {
    return concernInterface;
  }

  @Override
  public FeatureCategory getCategory() {
    return category;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Optional<FeatureProperty> get(Component component) {
    Component target = getTargetComponent(component);
    if (target == null) {
      return Optional.empty();
    }

    Object value = null;
    if (getter != null) {
      try {
        value = getter.apply((T) target);
      } catch (Exception e) {
        LOGGER.log(Level.FINE, "Failed to get property value", e);
      }
    }

    FeatureProperty.Builder builder = FeatureProperty.builder(displayName, getFeatureType());
    builder = builderConfig.apply(builder);
    builder.value(value);

    return Optional.of(builder.build());
  }

  @Override
  @SuppressWarnings("unchecked")
  public boolean set(Component component, Object value) {
    Component target = getTargetComponent(component);
    if (target == null || setter == null) {
      return false;
    }

    try {
      setter.accept((T) target, value);
      return true;
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to set property value", e);
      return false;
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Wraps the value with the key as {@code [key, value]} so the source generator can produce
   * two-argument method calls like {@code setStyle("flex-grow", "1")}.
   * </p>
   */
  @Override
  public Object getSourceValue(FeatureProperty property) {
    Object value = property.getValue();
    Class<?> javaType = property.getJavaType();

    String stringValue;
    if (value == null) {
      stringValue = "";
    } else if (javaType == Integer.class && value instanceof Number n) {
      stringValue = String.valueOf(n.intValue());
    } else {
      stringValue = String.valueOf(value);
    }
    return List.of(key, stringValue);
  }

  /**
   * Gets the key for this contribution.
   *
   * @return the key (e.g., "flex-grow")
   */
  protected String getKey() {
    return key;
  }

  /**
   * Sets the builder configuration function.
   *
   * <p>
   * Use this to configure the property type:
   * </p>
   * <ul>
   * <li>{@code setBuilderConfig(FeatureProperty.Builder::text)} for text</li>
   * <li>{@code setBuilderConfig(FeatureProperty.Builder::integer)} for integer</li>
   * <li>{@code setBuilderConfig(FeatureProperty.Builder::decimal)} for decimal</li>
   * <li>{@code setBuilderConfig(b -> b.select(options))} for select</li>
   * </ul>
   *
   * @param config the builder configuration function
   */
  protected void setBuilderConfig(UnaryOperator<FeatureProperty.Builder> config) {
    this.builderConfig = config;
  }

  /**
   * Sets the getter function.
   *
   * @param getter function to get value from component
   */
  protected void setGetter(Function<T, Object> getter) {
    this.getter = getter;
  }

  /**
   * Sets the setter function.
   *
   * @param setter function to set value on component
   */
  protected void setSetter(BiConsumer<T, Object> setter) {
    this.setter = setter;
  }
}
