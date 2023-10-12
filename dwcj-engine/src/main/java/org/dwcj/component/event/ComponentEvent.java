package org.dwcj.component.event;

import java.util.EventObject;
import java.util.Map;
import org.dwcj.component.Component;

/**
 * This class is the base class for all events fired by the controls.
 *
 * @param <T> the generic type
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ComponentEvent<T extends Component> extends EventObject {

  private transient Map<String, Object> eventMap;

  /**
   * Instantiates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public ComponentEvent(T control, Map<String, Object> eventMap) {
    super(control);
    this.eventMap = eventMap;
  }

  /**
   * Get the event map sent by the control.
   *
   * <p>
   * The event map is a serializable map from the original client event sent by the control.
   * </p>
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
   * Alias for {@link #getSource()} method.
   *
   * @return the component which fired the event
   */
  public Component getComponent() {
    return (Component) getSource();
  }
}
