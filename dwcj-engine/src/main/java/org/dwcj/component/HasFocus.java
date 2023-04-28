package org.dwcj.component;

/**
 * Interface that facilitates allowing a component to receive focus on the page.
 */
public interface HasFocus {

  /**
   * Gives a component focus when it is added to the window. Note that if this method is called on
   * multiple components, focus will be given to the component added latest to the window.
   *
   * @return The component itself
   */
  public HasFocus focus();
}
