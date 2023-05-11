package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is checked on or off.
 */
public class CheckChangedEvent extends Event<AbstractComponent> {
  public CheckChangedEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }


  /**
   * Gets the value of the component to determine wether it was checked on or off.
   *
   * @return a boolean representing wether it was checked on or off.
   */
  public boolean isChecked() {
    return (boolean) this.getEventMap().get("checked");
  }
}
