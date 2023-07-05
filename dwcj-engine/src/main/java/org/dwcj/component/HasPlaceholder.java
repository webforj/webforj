package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's placeholder text.
 *
 * @param <T> the type of the component
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasPlaceholder<T extends Component> {

  /**
   * Sets the placeholder text for the component.
   *
   * @param placeholder the placeholder text to set
   * @return the component itself
   */
  T setPlaceholder(String placeholder);

  /**
   * Gets the placeholder text for the component.
   *
   * @return the placeholder text
   */
  String getPlaceholder();
}
