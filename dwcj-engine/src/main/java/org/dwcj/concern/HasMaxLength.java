package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for modifying the maximum length of a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the maximum length allowed for the
 * component's value.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMaxLength<T extends Component> {

  /**
   * Retrieves the maximum length of the component's value.
   *
   * @return the maximum length
   */
  public int getMaxLength();

  /**
   * Sets the maximum length of the component's value.
   *
   * @param maxLength the maximum length to set for the component's value.
   * @return the component itself
   */
  public T setMaxLength(int maxLength);
}
