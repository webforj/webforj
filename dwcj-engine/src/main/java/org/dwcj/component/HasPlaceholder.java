package org.dwcj.component;

/**
 * Interface that facilitates implementation of behavior which determines whether a control
 * can have a placeholder. 
 */
public interface HasPlaceholder {

  /**
   * Sets whether the control displays a placeholder when empty.  
   *
   * @param placeholder the text displayed
   * @return the control itself
   */
  HasPlaceholder setPlaceholder(String placeholder);

  /**
   * Returns the placeholder text.
   *
   * @return the placeholder text
   */
  String getPlaceholder();
}
