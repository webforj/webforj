package org.dwcj.component;


/**
 * Interface which facilitates functionality to be implemented that controls whether or not a
 * control can gain and lose focus.
 */
public interface Focusable {

  /**
   * isFocusable returns a value indicating whether or not the user can focus the control. Note: A
   * focused control is the control to which keyboard interaction is directed. Typically, the
   * focused control is indicated visually, often via a dotted grey border. A control which is not
   * focusable, therefore, cannot be manipulated via the keyboard. It is still possible to interact
   * with a non-focusable control via the mouse.
   *
   * @return True if object can be focused, False if not.
   */
  Boolean isFocusable();

  /**
   * setFocusable sets whether a control can be focused.
   *
   * Note: A focused control is the control to which keyboard interaction is directed. Typically,
   * the focused control is indicated visually, often via a dotted grey border. A control which is
   * not focusable, therefore, cannot be manipulated via the keyboard. It is still possible to
   * interact with a non-focusable control via the mouse. To disable all interaction, including the
   * mouse, see setEnabled() and setReadOnly().
   *
   * @param focusable Boolean True for allowing focusing, False to disable focus
   * @return Object itself
   */
  Focusable setFocusable(Boolean focusable);

}
