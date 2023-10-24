package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when a component is toggled on or off.
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
