package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.List;

/**
 * The mock backing the edit box control.
 *
 * <p>
 * Driven by {@code com.webforj.component.field.DwcTextField} and its subclasses, which select text
 * ranges through the control and read the selection back as a four entry vector with the start
 * offset at index one and the end offset at index three.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjEditBoxMock extends BBjControlMock {
  private int selectionStart = 0;
  private int selectionEnd = 0;

  /**
   * Creates a new edit box mock.
   *
   * @param id the control identifier
   */
  BBjEditBoxMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "select" -> {
        selectionStart = (Integer) args[0];
        selectionEnd = (Integer) args[1];
        yield null;
      }
      case "getSelection" -> new BBjVector(List.of(0, selectionStart, 0, selectionEnd));
      case "getSelectedText" -> {
        String text = (String) getState("text");
        if (text == null || selectionStart < 0 || selectionEnd > text.length()
            || selectionStart >= selectionEnd) {
          yield "";
        }
        yield text.substring(selectionStart, selectionEnd);
      }
      default -> super.handle(proxy, method, args);
    };
  }
}
