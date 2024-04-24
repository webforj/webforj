package com.webforj.data.concern;

import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * An interface for modifying a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface ValueAware<T, V> {
  /**
   * Retrieves the value of the component.
   *
   * @return the value of the component.
   */
  public V getValue();

  /**
   * Sets the value of the component.
   *
   * @param value the value to set.
   * @return the component itself.
   */
  public T setValue(V value);

  /**
   * Adds a {@link ValueChangeEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ValueChangeEvent<V>> addValueChangeListener(
      EventListener<ValueChangeEvent<V>> listener);

  /**
   * Alias for {@link #addValueChangeListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return @return A registration object for removing the event listener
   */
  public default ListenerRegistration<ValueChangeEvent<V>> onValueChange(
      EventListener<ValueChangeEvent<V>> listener) {
    return addValueChangeListener(listener);
  }
}
