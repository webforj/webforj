package com.webforj.component.event.sink;

import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * The sink is responsible for setting and removing the callback on a BBjControl.
 *
 * @param <T> the type of the component associated with this event sink.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface DwcEventSink<T> {

  /**
   * Sets a callback on an underlying BBj control with the given options.
   *
   * @param options An options object
   *
   * @return the callback id.
   * @throws WebforjRuntimeException if the callback cannot be set.
   */
  public String setCallback(Object options);

  /**
   * Sets a callback on an underlying BBj control.
   *
   * @return the callback id.
   * @throws WebforjRuntimeException if the callback cannot be set.
   */
  public default String setCallback() {
    return setCallback(null);
  }

  /**
   * Removes a callback with the given id on an underlying BBj control.
   *
   * @param id the callback id.
   *
   * @throws WebforjRuntimeException if the callback cannot be removed.
   */
  public void removeCallback(String id);

  /**
   * Gets the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  public EventDispatcher getEventDispatcher();

  /**
   * Checks whether the control which the sink is responsible for is created or not.
   *
   * @return true if the control is created, false otherwise.
   */
  public default boolean isConnected() {
    return false;
  }

  /**
   * Checks whether the event sink supports multiple callbacks or not.
   *
   * @return true if the event sink supports multiple callbacks, false otherwise.
   */
  public default boolean isMultipleCallbacks() {
    return false;
  }

  /**
   * Gets the component associated with this event sink.
   *
   * @return the component associated with this event sink.
   */
  public T getComponent();
}
