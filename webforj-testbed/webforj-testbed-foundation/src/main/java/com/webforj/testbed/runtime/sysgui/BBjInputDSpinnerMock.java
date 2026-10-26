package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import java.lang.reflect.Method;
import java.time.LocalDate;

/**
 * The mock backing the date spinner, which steps the date by its spin field.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputDSpinnerMock extends BBjInputDMock {
  private final BBjSpinnerState spinner = new BBjSpinnerState(this::putText, this::step);

  BBjInputDSpinnerMock(int id) {
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
    LocalDate date = getDate();
    if (date == null) {
      return;
    }

    setDate(stepByField(date, up ? 1 : -1));
  }

  private LocalDate stepByField(LocalDate date, int amount) {
    int field = spinner.getSpinField();
    if (field == BBjInputDSpinner.YEAR) {
      return date.plusYears(amount);
    }

    if (field == BBjInputDSpinner.MONTH) {
      return date.plusMonths(amount);
    }

    if (field == BBjInputDSpinner.WEEK) {
      return date.plusWeeks(amount);
    }

    return date.plusDays(amount);
  }
}
