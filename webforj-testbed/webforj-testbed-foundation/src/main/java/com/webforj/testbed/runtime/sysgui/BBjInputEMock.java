package com.webforj.testbed.runtime.sysgui;



/**
 * The mock backing the masked text input control.
 *
 * <p>
 * Driven by {@code com.webforj.component.field.MaskedTextField}, which only uses the masked input
 * surface.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputEMock extends BBjInputMock {

  /**
   * Creates a new masked text input mock.
   *
   * @param id the control identifier
   */
  BBjInputEMock(int id) {
    super(id);
  }
}
