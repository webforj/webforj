package com.webforj.component.layout.appnav;

import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.element.ElementCompositeContainer;
import java.util.ArrayList;
import java.util.List;

/**
 * A container for navigation items.
 *
 * @param <T> the type of the container
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see AppNavItem
 * @see AppNav
 */
abstract class NavigationContainer<T extends ElementCompositeContainer>
    extends ElementCompositeContainer {
  private List<AppNavItem> items = new ArrayList<>();

  /**
   * Adds a new navigation item.
   *
   * @param item the navigation item to add
   * @return the component itself
   */
  public T addItem(AppNavItem item) {
    if (item.isDestroyed()) {
      throw new IllegalStateException("Cannot add a destroyed item");
    }

    if (items.contains(item)) {
      return getSelf();
    }

    items.add(item);
    item.addLifecycleObserver((component, event) -> {
      if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
        items.remove(item);
      }
    });

    String slotName = getSlotName();
    getElement().add(slotName != null && !slotName.isEmpty() ? slotName : "", item);
    return getSelf();
  }

  /**
   * Removes a navigation item.
   *
   * @param item the navigation item to remove
   * @return the component itself
   */
  public T removeItem(AppNavItem item) {
    if (!items.contains(item)) {
      return getSelf();
    }

    items.remove(item);
    item.destroy();
    return getSelf();
  }

  /**
   * Returns the name of the slot where the navigation items are placed.
   *
   * @return the name of the slot
   */
  protected abstract String getSlotName();

  /**
   * Returns an instance of the current class, casted to its generic type. This method is primarily
   * used for method chaining in subclasses.
   *
   * @return An instance of the current class, casted to its generic type.
   */
  private final T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}
