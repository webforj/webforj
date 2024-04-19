package com.webforj.data.event;

import java.util.EventObject;

/**
 * An event that is fired when a component is edited or modified.
 *
 * @param <V> the type of the value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public class ValueChangeEvent<V> extends EventObject {
  private transient V value;

  /**
   * Constructs a new {@code ValueChangeEvent} event.
   *
   * @param component the component that fired the event.
   * @param value the value of the component.
   */
  public ValueChangeEvent(Object component, V value) {
    super(component);
    this.value = value;
  }

  /**
   * Gets the value of the component.
   *
   * @return the value of the component.
   */
  public V getValue() {
    return value;
  }
}
