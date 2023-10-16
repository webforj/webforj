package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for modifying the minimum length of a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the minimum length required for the
 * component's value.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMinLength<T extends Component> {

  /**
   * Retrieves the minimum length required for the component's value.
   *
   * @return the minimum length required for the component's value.
   */
  public int getMinLength();

  /**
   * Sets the minimum length required for the component's value.
   *
   * @param minLength the minimum length to set for the component's value
   * @return the component itself.
   */
  public T setMinLength(int minLength);
}
