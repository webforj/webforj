package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's value minimum length.
 *
 * @param <T> the type of the component
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMinLength<T extends Component> {

  /**
   * Returns the minimum length of the component's value.
   *
   * @return the minimum length of the component value
   */
  public int getMinLength();

  /**
   * Set the minimum length of the component value.
   *
   * @param minLength the minimum length to set for the component value
   * @return the control itself
   */
  public T setMinLength(int minLength);
}
