package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's value maximum.
 *
 * @param <T> the type of the component
 * @param <V> the type of the maximum value
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMax<T extends Component, V> {

  /**
   * Set the maximum value for the component's value.
   *
   * @param max the maximum value
   *
   * @return the component itself
   * @since 23.02
   */
  public T setMax(V max);

  /**
   * Returns the maximum value for the component's value.
   *
   * @return the maximum value for the component's value
   * @since 23.02
   */
  public V getMax();
}
