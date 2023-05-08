package org.dwcj.component;

/**
 * Interface for a component that can beep when clicked.
 */
public interface HasBeep {

  /**
   * Set wether the control should beep when clicked.
   *
   * @param beep true if the control should beep, false otherwise
   * @return the control itself
   */
  public HasBeep setBeep(boolean beep);

  /**
   * Returns wether the control beeps.
   *
   * @return true if the control beeps, false otherwise
   */
  public boolean getBeep();
}
