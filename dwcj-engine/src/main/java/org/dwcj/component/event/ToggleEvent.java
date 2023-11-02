package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when an element or control changes its state between two possible states,
 * such as "on" and "off" or "visible" and "hidden." It can apply to various elements, such as 
 * buttons, dropdowns, menus, or modals. When a "toggle" event is triggered, the element switches 
 * its state from one option to another, reflecting the user's action or changing conditions.
 * 
 * @see CheckEvent
 * @see UncheckEvent
 */
public class ToggleEvent extends ComponentEvent<Component> {
  public ToggleEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the value of the component to determine whether it was toggled on or off.
   *
   * @return a boolean returns true if toggled on, false if toggled off.
   */
  public boolean isToggled() {
    return (boolean) this.getEventMap().get("toggled");
  }
}
