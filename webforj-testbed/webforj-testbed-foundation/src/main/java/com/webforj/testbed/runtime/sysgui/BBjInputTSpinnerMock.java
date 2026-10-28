package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.sysgui.BBjInputTSpinner;
import java.lang.reflect.Method;
import java.time.LocalTime;

/**
 * The mock backing the time spinner, which steps the time by its spin field.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputTSpinnerMock extends BBjInputTMock {
  private final BBjSpinnerState spinner = new BBjSpinnerState(this::putText, this::step);

  BBjInputTSpinnerMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    Object result = spinner.handle(method.getName(), args);
    if (result != BBjSpinnerState.UNHANDLED_SENTINEL) {
      return result;
    }

    return super.handle(proxy, method, args);
  }

  private void step(boolean up) {
    LocalTime time = getTime();
    if (time == null) {
      return;
    }

    setTime(stepByField(time, up ? 1 : -1));
  }

  private LocalTime stepByField(LocalTime time, int amount) {
    int field = spinner.getSpinField();
    if (field == BBjInputTSpinner.MINUTE) {
      return time.plusMinutes(amount);
    }

    if (field == BBjInputTSpinner.SECOND) {
      return time.plusSeconds(amount);
    }

    return time.plusHours(amount);
  }
}
