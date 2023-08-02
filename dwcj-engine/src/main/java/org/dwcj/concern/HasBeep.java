package org.dwcj.concern;

/**
 * Interface for a component that can beep when clicked.
 */
public interface HasBeep {

  /**
   * Set whether the component should beep when clicked.
   *
   * @param beep true if the control should beep, false otherwise
   * @return the control itself
   */
  public HasBeep setBeep(boolean beep);

  /**
   * Returns whether the component beeps.
   *
   * @return true if the component beeps, false otherwise
   */
  public boolean getBeep();
}
