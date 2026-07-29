package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Base class for list-based contributions like HasClassName.
 *
 * <p>
 * Handles add/remove actions for list properties. The client sends actions in a standardized
 * format:
 * </p>
 *
 * <pre>
 * {
 *   "action": "add" | "remove",
 *   "item": "itemValue"
 * }
 * </pre>
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * public class HasClassNameContribution extends ListConcernContribution&lt;HasClassName&lt;?&gt;&gt; {
 *   public HasClassNameContribution() {
 *     super(HasClassName.class, "classNames", FeatureCategory.STYLING);
 *     setGetter(c -&gt; null); // Values fetched from DOM
 *     setAddHandler((c, item) -&gt; c.addClassName(item));
 *     setRemoveHandler((c, item) -&gt; c.removeClassName(item));
 *   }
 * }
 * </pre>
 *
 * @param <T> the concern interface type
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class ListConcernContribution<T> implements FeatureHandler {

  private static final Logger LOGGER = Logger.getLogger(ListConcernContribution.class.getName());

  private final Class<?> concernInterface;
  private final String propertyName;
  private final FeatureCategory category;
  private Function<T, Object> getter;
  private BiConsumer<T, String> addHandler;
  private BiConsumer<T, String> removeHandler;

  /**
   * Creates a new list contribution.
   *
   * @param concernInterface the concern interface class
   * @param propertyName the property name
   * @param category the feature category
   */
  protected ListConcernContribution(Class<?> concernInterface, String propertyName,
      FeatureCategory category) {
    this.concernInterface = concernInterface;
    this.propertyName = propertyName;
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

    Object value = getter != null ? getter.apply((T) target) : null;
    return Optional
        .of(FeatureProperty.builder(propertyName, getFeatureType()).list().value(value).build());
  }

  @Override
  @SuppressWarnings("unchecked")
  public boolean set(Component component, Object value) {
    Component target = getTargetComponent(component);
    if (target == null) {
      return false;
    }

    if (!(value instanceof Map<?, ?> actionMap)) {
      return false;
    }

    String action = getString(actionMap, "action");
    String item = getString(actionMap, "item");

    if (item == null || item.isEmpty()) {
      return false;
    }

    try {
      return switch (action) {
        case "add" -> {
          if (addHandler != null) {
            addHandler.accept((T) target, item);
          }
          yield true;
        }
        case "remove" -> {
          if (removeHandler != null) {
            removeHandler.accept((T) target, item);
          }
          yield true;
        }
        default -> false;
      };
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to set list property on component", e);
      return false;
    }
  }

  /**
   * Sets the getter function for the list value.
   *
   * @param getter function to get list value from component (can return null if fetched from DOM)
   */
  protected void setGetter(Function<T, Object> getter) {
    this.getter = getter;
  }

  /**
   * Sets the handler for adding items.
   *
   * @param handler function to add an item to the component
   */
  protected void setAddHandler(BiConsumer<T, String> handler) {
    this.addHandler = handler;
  }

  /**
   * Sets the handler for removing items.
   *
   * @param handler function to remove an item from the component
   */
  protected void setRemoveHandler(BiConsumer<T, String> handler) {
    this.removeHandler = handler;
  }

  private String getString(Map<?, ?> map, String key) {
    if (map == null || key == null) {
      return null;
    }

    Object val = map.get(key);
    return (val instanceof String v) ? v : null;
  }
}
