package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface that enables modification of a component's maximum value.
 *
 * <p>
 * This interface provides methods to set and retrieve the maximum possible value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the maximum value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMax<T extends Component, V> {

  /**
   * Sets the maximum possible value for the component.
   *
   * @param max the maximum value to set
   *
   * @return the component itself
   */
  public T setMax(V max);

  /**
   * Retrieves the maximum possible value for the component.
   *
   * @return the maximum value of the component.
   */
  public V getMax();
}
