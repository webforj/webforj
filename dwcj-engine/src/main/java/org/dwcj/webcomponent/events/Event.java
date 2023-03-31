package org.dwcj.webcomponent.events;

import java.util.EventObject;
import java.util.Map;

import org.dwcj.component.AbstractComponent;
import org.dwcj.interfaces.Control;
import org.dwcj.interfaces.ControlEvent;

/**
 * This class is the base class for all events fired by the controls.
 * 
 * @param <T> the generic type
 * 
 * @author Hyyan Abo Fakher
 */
public class Event<T extends Control> extends EventObject implements ControlEvent {

  private Map<String, Object> eventMap;

  /**
   * Instantiates a new event.
   * 
   * @param control  the control
   * @param eventMap the event map
   */
  public Event(T control, Map<String, Object> eventMap) {
    super(control);
    this.eventMap = eventMap;
  }

  /**
   * Get the event map sent by the control
   * 
   * The event map is a serializable map from the original client event sent by
   * the control.
   * 
   * @return the event map
   */
  public Map<String, Object> getData() {
    return eventMap;
  }

  /**
   * Alias for {@link #getData()} method.
   * 
   * @return the event map
   */
  public Map<String, Object> getEventMap() {
    return getData();
  }

  /**
   * Gets the control.
   * 
   * @return the control
   */
  @Override
  public AbstractComponent getControl() {
    return (AbstractComponent) getSource();
  }
}