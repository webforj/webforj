package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is scrolled.
 */
public class ComponentScrollEvent extends Event<AbstractComponent> {
  
  
  /**
   * Creates a new event.
   *
   * @param component the component that fired the event.
   * @param payload the event map.
   * 
   */
  public ComponentScrollEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the orientation of the component that is sent as part of the event payload.
   *
   * @return the orientation of the component as an int.
   */
  public int getOrientation() {
    return (int) this.getEventMap().get("orientation");
  }

  /**
   * Gets the position of the component that is sent as part of the event payload.
   *
   * @return the position of the component as an int.
   */
  public int getPosition() {
    return (int) this.getEventMap().get("position");
  }

  /**
   * Gets wether or not the component is adjusting.
   *
   * @return a boolean representing wether the component is adjusting.
   */
  public boolean isAdjusting() {
    return (boolean) this.getEventMap().get("adjusting");
  }

}
