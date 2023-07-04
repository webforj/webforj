package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's value minimum.
 *
 * @param <T> the type of the component
 * @param <V> the type of the minimum value
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMin<T extends Component, V> {

  /**
   * Set the minimum value for the component's value.
   *
   * @param min the minimum value
   *
   * @return the component itself
   * @since 23.02
   */
  public T setMin(V min);

  /**
   * Returns the minimum value for the component's value.
   *
   * @return the minimum value for the component's value
   * @since 23.02
   */
  public V getMin();
}
