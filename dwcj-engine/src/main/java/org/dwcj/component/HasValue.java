package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component value.
 *
 * @param <T> the type of the component
 * @param <V> the type of the value
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasValue<T, V> {

  /**
   * Returns the value of the component.
   *
   * @return the value of the component
   */
  public V getValue();

  /**
   * Set the value of the component.
   *
   * @param value the value to set
   * @return the control itself
   */
  public T setValue(V value);
}
