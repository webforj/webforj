package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's value maximum length.
 *
 * @param <T> the type of the component
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMaxLength<T extends Component> {

  /**
   * Returns the maximum length of the component's value.
   *
   * @return the maximum length of the component value
   */
  public int getMaxLength();

  /**
   * Set the maximum length of the component value.
   *
   * @param maxLength the maximum length to set for the component value
   * @return the control itself
   */
  public T setMaxLength(int maxLength);
}
