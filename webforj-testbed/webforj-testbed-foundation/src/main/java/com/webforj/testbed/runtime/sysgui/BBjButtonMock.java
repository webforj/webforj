package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the button control.
 *
 * <p>
 * Driven by {@code com.webforj.component.button.DwcButton}, which writes and reads the disable on
 * click flag through the control.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjButtonMock extends BBjControlMock {
  private boolean disableOnClick = false;

  /**
   * Creates a new button mock.
   *
   * @param id the control identifier
   */
  BBjButtonMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setDisableOnClick" -> {
        disableOnClick = (Boolean) args[0];
        yield null;
      }
      case "getDisableOnClick" -> disableOnClick;
      default -> super.handle(proxy, method, args);
    };
  }
}
