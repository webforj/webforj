package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the masked input control surface.
 *
 * <p>
 * Driven by {@code com.webforj.component.field.DwcMaskedField}, which writes the mask, the restore
 * value, the typing mode and the caret position through the control and selects text ranges.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputMock extends BBjControlMock {
  private String mask = "";
  private String restore = "";
  private boolean insertMode = false;
  private int caretPosition = 0;

  /**
   * Creates a new masked input mock.
   *
   * @param id the control identifier
   */
  BBjInputMock(int id) {
    super(id);
  }

  /**
   * Returns the mask written through the control.
   *
   * @return the mask
   */
  String getMask() {
    return mask;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setMask" -> {
        mask = (String) args[0];
        yield null;
      }
      case "getMask" -> mask;
      case "setRestore" -> {
        restore = (String) args[0];
        yield null;
      }
      case "getRestore" -> restore;
      case "setInsertMode" -> {
        insertMode = (Boolean) args[0];
        yield null;
      }
      case "getInsertMode" -> insertMode;
      case "setCaretPosition" -> {
        caretPosition = (Integer) args[0];
        yield null;
      }
      case "getCaretPosition" -> caretPosition;
      case "select" -> null;
      default -> super.handle(proxy, method, args);
    };
  }
}
