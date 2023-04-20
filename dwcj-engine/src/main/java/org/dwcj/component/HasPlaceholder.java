package org.dwcj.component;

/**
 * The HasPlaceholder interface provides methods to set and get the placeholder text for a component
 * that displays a placeholder when empty.
 */
public interface HasPlaceholder {

  /**
   * Sets the text to be displayed as the placeholder for the component.
   *
   * @param placeholder the text displayed
   * @return the control itself
   */
  HasPlaceholder setPlaceholder(String placeholder);

  /**
   * Returns the placeholder text currently set for the control.
   *
   * @return the placeholder text
   */
  String getPlaceholder();
}
