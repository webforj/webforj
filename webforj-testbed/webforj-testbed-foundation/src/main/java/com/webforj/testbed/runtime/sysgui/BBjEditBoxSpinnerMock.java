package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the text spinner, whose list drives its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjEditBoxSpinnerMock extends BBjEditBoxMock {
  private final BBjSpinnerState spinner = new BBjSpinnerState(this::putText, null);

  BBjEditBoxSpinnerMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    Object result = spinner.handle(method.getName(), args);
    if (result != BBjSpinnerState.UNHANDLED_SENTINEL) {
      return result;
    }

    // Text equal to a list entry moves the list to it.
    if (method.getName().equals("setText")) {
      int index = spinner.indexOf((String) args[0]);
      if (index >= 0) {
        spinner.setListIndexSilently(index);
      }
    }

    return super.handle(proxy, method, args);
  }
}
