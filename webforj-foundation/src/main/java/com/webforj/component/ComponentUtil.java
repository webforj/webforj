package com.webforj.component;

import com.webforj.component.event.ComponentEvent;
import java.util.EventObject;
import java.util.Objects;

/**
 * Utility methods for {@link Component}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class ComponentUtil {

  private ComponentUtil() {}

  /**
   * Retrieves the bound component of the given component.
   *
   * @param component the component to retrieve its bound component
   * @return the bound component
   */
  public static Component getBoundComponent(Object component) {
    if (!(component instanceof Composite)) {
      throw new IllegalArgumentException("The component must be an instance of Composite");
    }

    return ((Composite<?>) component).getBoundComponent();
  }

  /**
   * Delivers the given event to the listeners of the given component, as the client sends it.
   *
   * <p>
   * Nothing happens when the component has no listener for the event. An element composite delivers
   * a {@link ComponentEvent} with an event name rebuilt from that name and its data, as when the
   * client sends it.
   * </p>
   *
   * @param component the component the event is delivered to
   * @param event the event to deliver
   *
   * @throws NullPointerException if the component or the event is null
   * @throws IllegalArgumentException if the event belongs to another component
   * @throws IllegalStateException if the component delivers no events to listeners
   *
   * @since 26.03
   */
  public static void fireComponentEvent(Component component, EventObject event) {
    Objects.requireNonNull(component, "The component must not be null");
    Objects.requireNonNull(event, "The event must not be null");

    if (event.getSource() != component) {
      throw new IllegalArgumentException(event.getClass().getSimpleName()
          + " belongs to another component. Fire it on the component it was created for.");
    }

    component.fireComponentEvent(event);
  }
}
