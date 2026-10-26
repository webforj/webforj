package com.webforj.testbed.runtime.sysgui;



/**
 * The mock backing the navigator control.
 *
 * <p>
 * Driven by {@code com.webforj.component.navigator.Navigator}, which pushes its whole state through
 * properties and only registers event callbacks on the control, both covered by the base control
 * surface.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjNavigatorMock extends BBjControlMock {

  /**
   * Creates a new navigator mock.
   *
   * @param id the control identifier
   */
  BBjNavigatorMock(int id) {
    super(id);
  }
}
