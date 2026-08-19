package com.webforj.component.layout.appnav;

import com.webforj.component.Component;
import com.webforj.component.element.Element;

/**
 * Holds the single component of a named slot.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class SlottedComponent {
  private Component component;

  /**
   * Returns the component currently held by the slot.
   *
   * @return the slot component, or {@code null} when none was set
   */
  Component get() {
    return component;
  }

  /**
   * Places the given component in the slot.
   *
   * @param element the element owning the slot
   * @param slot the name of the slot
   * @param value the component to place
   */
  void set(Element element, String slot, Component value) {
    if (value.equals(component)) {
      return;
    }

    if (component != null) {
      component.destroy();
    }

    component = value;
    element.add(slot, value);
  }
}
