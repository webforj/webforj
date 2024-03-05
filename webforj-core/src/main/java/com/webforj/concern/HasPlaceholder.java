package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface for modifying a component's placeholder text.
 *
 * <p>
 * This interface provides methods to set and retrieve the placeholder text for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasPlaceholder<T extends Component> {

  /**
   * Sets the placeholder text for the component.
   *
   * @param placeholder the placeholder text to set
   * @return the component itself after setting the placeholder text.
   */
  T setPlaceholder(String placeholder);

  /**
   * Retrieves the placeholder text for the component.
   *
   * @return the placeholder text for the component.
   */
  String getPlaceholder();
}
