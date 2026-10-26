package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the static text control.
 *
 * <p>
 * Driven by {@code com.webforj.component.text.Label}, which writes the line wrap flag through the
 * control.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjStaticTextMock extends BBjControlMock {
  private boolean lineWrap = true;

  /**
   * Creates a new static text mock.
   *
   * @param id the control identifier
   */
  BBjStaticTextMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setLineWrap" -> {
        lineWrap = (Boolean) args[0];
        yield null;
      }
      case "getLineWrap" -> lineWrap;
      default -> super.handle(proxy, method, args);
    };
  }
}
