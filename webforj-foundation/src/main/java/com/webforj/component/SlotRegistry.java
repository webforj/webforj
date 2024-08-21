package com.webforj.component;

import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * {@code SlotRegistry} manages components by associating them with named slots.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class SlotRegistry {
  static final String DEFAULT_SLOT = "__default__";
  private final Map<String, List<Component>> slots = new LinkedHashMap<>();

  /**
   * Adds the given components to the specified slot.
   *
   * <p>
   * If the slot name is null or empty, the components will be added to the default slot.
   * </p>
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @param components the components to be added.
   * @throws IllegalArgumentException if a component is already assigned to a slot.
   */
  public void addComponentsToSlot(String slot, Component... components) {
    String resolvedSlot = resolveSlotName(slot);

    for (Component component : components) {
      // Check if the component is already in a slot
      String currentSlot = findComponentSlot(component);
      if (currentSlot != null) {
        throw new IllegalArgumentException("Component is already assigned to slot.");
      }

      slots.computeIfAbsent(resolvedSlot, k -> new ArrayList<>()).add(component);
      component.addLifecycleObserver((c, event) -> {
        if (event == LifecycleEvent.DESTROY) {
          removeComponentsFromSlot(c);
        }
      });
    }
  }

  /**
   * Replaces the content of the specified slot with new components.
   *
   * <p>
   * All existing components in the slot are destroyed before adding the new ones.
   * </p>
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @param components the new components to be added.
   *
   * @throws IllegalArgumentException if any of the components is already assigned to a slot.
   */
  public void replaceComponentsInSlot(String slot, Component... components) {
    removeSlot(slot);
    addComponentsToSlot(slot, components);
  }

  /**
   * Removes the given components from their associated slots.
   *
   * @param components the components to be removed.
   */
  public void removeComponentsFromSlot(Component... components) {
    for (Component component : components) {
      String slot = findComponentSlot(component);
      if (!component.isDestroyed()) {
        component.destroy();
      }

      List<Component> slotRef = slots.get(resolveSlotName(slot));
      if (slotRef != null) {
        slotRef.remove(component);
      }
    }
  }

  /**
   * Removes all components from the specified slot.
   *
   * @param slot the slot name, or null/empty for the default slot.
   */
  public void removeSlot(String slot) {
    // Destroy all components in the slot
    getComponentsInSlot(slot).forEach(Component::destroy);
    slots.remove(resolveSlotName(slot));
  }

  /**
   * Finds which slot a given component belongs to.
   *
   * @param component the component to search for.
   * @return the name of the slot containing the component, or an empty string if not found.
   */
  public String findComponentSlot(Component component) {
    for (Map.Entry<String, List<Component>> entry : slots.entrySet()) {
      if (entry.getValue().contains(component)) {
        return entry.getKey().equals(DEFAULT_SLOT) ? "" : entry.getKey();
      }
    }

    return null;
  }

  /**
   * Returns a list of components assigned to the given slot.
   *
   * <p>
   * If the slot name is null or empty, the default slot will be used.
   * </p>
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @return the list of components in the specified slot, or an empty list if none exist.
   */
  public List<Component> getComponentsInSlot(String slot) {
    return slots.getOrDefault(resolveSlotName(slot), Collections.emptyList());
  }

  /**
   * Returns a list of components of the specified type assigned to the given slot.
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @param classOfT the class type to filter by.
   * @param <T> the type of component.
   *
   * @return the list of components of the given type in the slot.
   */
  public <T extends Component> List<T> getComponentsInSlot(String slot, Class<T> classOfT) {
    return slots.getOrDefault(resolveSlotName(slot), Collections.emptyList()).stream()
        .filter(classOfT::isInstance).map(classOfT::cast).toList();
  }

  /**
   * Gets the first component in the specified slot.
   *
   * <p>
   * If the slot name is null or empty, the default slot will be used.
   * </p>
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @return the first component in the slot, or null if the slot is empty.
   */
  public Component getFirstComponentInSlot(String slot) {
    return getComponentsInSlot(slot).stream().findFirst().orElse(null);
  }

  /**
   * Gets the first component of the specified type in the slot.
   *
   * <p>
   * If the slot name is null or empty, the default slot will be used.
   * </p>
   *
   * @param slot the slot name, or null/empty for the default slot.
   * @param classOfT the class type to filter by.
   * @param <T> the type of component.
   *
   * @return the first component of the given type in the slot, or null if none is found.
   */
  public <T extends Component> T getFirstComponentInSlot(String slot, Class<T> classOfT) {
    return getComponentsInSlot(slot, classOfT).stream().findFirst().orElse(null);
  }

  /**
   * Resolves the slot name, returning the default slot if the given slot name is null or empty.
   *
   * @param slot the slot name to resolve.
   * @return the resolved slot name, or the default slot if null or empty.
   */
  private String resolveSlotName(String slot) {
    return (slot == null || slot.trim().isEmpty()) ? DEFAULT_SLOT : slot;
  }
}
