package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Base class for concern-based contributions.
 *
 * <p>
 * Extend this class to define properties for a concern interface like HasText, HasTheme, etc. One
 * contribution per concern, works for all components implementing that concern.
 * </p>
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * public class HasTextContribution extends ConcernContribution&lt;HasText&lt;?&gt;&gt; {
 *   public HasTextContribution() {
 *     super(HasText.class, "Text", FeatureCategory.CONTENT);
 *     setBuilderConfig(FeatureProperty.Builder::text);
 *     setGetter(HasText::getText);
 *     setSetter((c, v) -&gt; c.setText(String.valueOf(v)));
 *   }
 * }
 * </pre>
 *
 * @param <T> the concern interface type
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class ConcernContribution<T> implements FeatureHandler {

  private static final Logger LOGGER = Logger.getLogger(ConcernContribution.class.getName());

  private final Class<?> concernInterface;
  private final String propertyName;
  private final FeatureCategory category;
  private UnaryOperator<FeatureProperty.Builder> builderConfig = FeatureProperty.Builder::text;
  private Function<T, Object> getter;
  private BiConsumer<T, Object> setter;

  /**
   * Creates a new contribution for the given concern interface.
   *
   * @param concernInterface the concern interface class
   * @param propertyName the property name
   * @param category the feature category
   */
  protected ConcernContribution(Class<?> concernInterface, String propertyName,
      FeatureCategory category) {
    this.concernInterface = concernInterface;
    this.propertyName = propertyName;
    this.category = category;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<?> getFeatureInterface() {
    return concernInterface;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FeatureCategory getCategory() {
    return category;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  public Optional<FeatureProperty> get(Component component) {
    Component target = getTargetComponent(component);
    if (target == null || getter == null) {
      return Optional.empty();
    }

    try {
      Object value = getter.apply((T) target);
      FeatureProperty.Builder builder = FeatureProperty.builder(propertyName, getFeatureType());
      builder = builderConfig.apply(builder);
      builder.value(value);
      return Optional.of(builder.build());
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to get property from component", e);
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
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
      LOGGER.log(Level.FINE, "Failed to set property on component", e);
      return false;
    }
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
   * <li>{@code setBuilderConfig(FeatureProperty.Builder::bool)} for boolean</li>
   * <li>{@code setBuilderConfig(b -> b.integer(0, 100))} for bounded integer</li>
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
  @SuppressWarnings("unchecked")
  protected void setGetter(Function<T, ?> getter) {
    this.getter = (Function<T, Object>) getter;
  }

  /**
   * Sets the setter function.
   *
   * @param setter function to set value on component
   */
  @SuppressWarnings("unchecked")
  protected void setSetter(BiConsumer<T, ?> setter) {
    this.setter = (BiConsumer<T, Object>) setter;
  }
}
