package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureGroup;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceLoader;

/**
 * Registry for feature handlers.
 *
 * <p>
 * All handlers (core and custom) are discovered via {@link ServiceLoader}. To register a custom
 * handler:
 * </p>
 * <ol>
 * <li>Extend {@link ConcernContribution}, {@link EnumConcernContribution}, or
 * {@link ListConcernContribution}</li>
 * <li>Create a file under META-INF/services named after the fully qualified name of
 * {@link FeatureHandler}</li>
 * <li>Add the fully qualified class name of your contribution to that file</li>
 * </ol>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FeatureHandlerRegistry {

  private final Map<String, FeatureHandler> handlers = new LinkedHashMap<>();

  /**
   * Creates a new registry and loads all handlers via ServiceLoader.
   */
  public FeatureHandlerRegistry() {
    loadHandlers();
  }

  /**
   * Extracts all feature groups from a component.
   *
   * @param component the component to extract from
   * @return a list of FeatureGroup objects with all extracted features
   */
  public List<FeatureGroup> getFeatureGroups(Component component) {
    return getFeatureGroups(component, null);
  }

  /**
   * Extracts all feature groups from a component, filtered by parent type.
   *
   * @param component the component to extract from
   * @param parentType the fully qualified class name of the parent component, or null
   * @return a list of FeatureGroup objects with all extracted features
   */
  public List<FeatureGroup> getFeatureGroups(Component component, String parentType) {
    List<FeatureGroup> result = new ArrayList<>();

    // Determine if this component is the root (its class matches the declaring class).
    // For root components, we allow falling back to bound component features.
    // For nested components, we only show features the component directly implements.
    boolean allowBoundFallback = isRootComponent(component);

    // Group properties by their category
    Map<FeatureCategory, List<FeatureProperty>> groupedProperties =
        new EnumMap<>(FeatureCategory.class);

    for (FeatureHandler handler : handlers.values()) {
      if (handler.supports(component, allowBoundFallback) && handler.supportsParent(parentType)) {
        Optional<FeatureProperty> propertyOpt = handler.get(component);
        if (propertyOpt.isPresent()) {
          FeatureCategory category = handler.getCategory();
          groupedProperties.computeIfAbsent(category, k -> new ArrayList<>())
              .add(propertyOpt.get());
        }
      }
    }

    // Create feature groups sorted by category order (enum ordinal)
    List<FeatureCategory> sortedCategories = new ArrayList<>(groupedProperties.keySet());
    sortedCategories.sort(Comparator.comparingInt(FeatureCategory::getOrder));

    for (FeatureCategory category : sortedCategories) {
      List<FeatureProperty> properties = groupedProperties.get(category);
      FeatureGroup group = new FeatureGroup(category.getId(), category.getLabel());

      for (FeatureProperty prop : properties) {
        group.addProperty(prop);
      }

      result.add(group);
    }

    return result;
  }

  /**
   * Applies a property change to a component.
   *
   * @param component the component to modify
   * @param featureType the feature type
   * @param value the new value
   * @return true if the change was applied successfully
   */
  public boolean applyChange(Component component, String featureType, Object value) {
    return applyChange(component, null, featureType, value);
  }

  /**
   * Applies a property change to a component, with the parent component available.
   *
   * <p>
   * Parent-scoped handlers apply the change through the parent's API (e.g.
   * {@code flexLayout.setItemGrow(1, item)}). The parent is resolved by the client, which owns the
   * component tree.
   * </p>
   *
   * @param component the component to modify
   * @param parent the parent component, or null when unknown
   * @param featureType the feature type
   * @param value the new value
   * @return true if the change was applied successfully
   */
  public boolean applyChange(Component component, Component parent, String featureType,
      Object value) {
    FeatureHandler handler = handlers.get(featureType);
    if (handler == null || !handler.supports(component)) {
      return false;
    }

    return handler.set(component, parent, value);
  }

  /**
   * Gets a handler by feature type.
   *
   * @param featureType the feature type identifier
   * @return an Optional containing the handler if found
   */
  public Optional<FeatureHandler> getHandler(String featureType) {
    return Optional.ofNullable(handlers.get(featureType));
  }

  /**
   * Gets all registered feature handlers.
   *
   * @return an unmodifiable view of the registered handlers
   */
  public Collection<FeatureHandler> getHandlers() {
    return Collections.unmodifiableCollection(handlers.values());
  }

  /**
   * Determines if a component is the root component (same class as the declaring class).
   *
   * <p>
   * A component is considered "root" when its class matches the class where it was instantiated.
   * For example, when {@code LoginView} is inspected and was created in {@code LoginView.java},
   * it's the root. When {@code Login} is created inside {@code LoginView.java}, it's nested.
   * </p>
   *
   * <p>
   * Root components can have their bound component features shown (for Composites), because we can
   * generate code like {@code getBoundComponent().setX()}. Nested components can only show features
   * they directly implement, because we can only call their public API.
   * </p>
   *
   * @param component the component to check
   * @return true if the component is the root, false if nested or unknown
   */
  private boolean isRootComponent(Component component) {
    SourcePoint sourcePoint = ComponentSourceRegistry.getSourcePoint(component);
    if (sourcePoint == null) {
      // Cannot determine source location, treat as nested
      return false;
    }

    return component.getClass().getName().equals(sourcePoint.className());
  }

  private void loadHandlers() {
    ServiceLoader<FeatureHandler> loader = ServiceLoader.load(FeatureHandler.class);
    for (FeatureHandler handler : loader) {
      handlers.put(handler.getFeatureType(), handler);
    }
  }
}
