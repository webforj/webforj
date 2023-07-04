package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's minimum value.
 *
 * @param <T> the type of the component
 * @param <V> the type of the minimum value
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMin<T extends Component, V> {

  /**
   * Set the minimum possible value for the component.
   *
   * @param min the minimum value
   *
   * @return the component itself
   * @since 23.02
   */
  public T setMin(V min);

  /**
   * Returns the minimum possible value for the component.
   *
   * @return the minimum component value
   * @since 23.02
   */
  public V getMin();
}
