package com.webforj.testbed.runtime.sysgui;



/**
 * The mock backing the check box control.
 *
 * <p>
 * Driven by {@code com.webforj.component.optioninput.CheckBox}, which only uses the toggle button
 * surface.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjCheckBoxMock extends BBjToggleButtonMock {

  /**
   * Creates a new check box mock.
   *
   * @param id the control identifier
   */
  BBjCheckBoxMock(int id) {
    super(id);
  }
}
