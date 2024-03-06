package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.EventObject;
import java.util.Map;

/**
 * This class is the base class for all events fired by the components.
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
   * @param component the component
   * @param eventMap the event map
   */
  public ComponentEvent(T component, Map<String, Object> eventMap) {
    super(component);
    this.eventMap = eventMap;
  }

  /**
   * Gets the event map sent by the component.
   *
   * <p>
   * The event map is a serializable map from the original client event sent by the component.
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
