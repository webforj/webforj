package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the slider control.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjSliderMock extends BBjControlMock {
  private int value = 0;
  private int minimum = 0;
  private int maximum = 100;

  BBjSliderMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setValue" -> {
        value = (Integer) args[0];
        yield null;
      }
      case "getValue" -> value;
      case "setMinimum" -> {
        minimum = (Integer) args[0];
        yield null;
      }
      case "getMinimum" -> minimum;
      case "setMaximum" -> {
        maximum = (Integer) args[0];
        yield null;
      }
      case "getMaximum" -> maximum;
      case "setMajorTickSpacing", "setMinorTickSpacing", "setSnapToTicks", "setPaintTicks",
          "setInverted", "setLabels", "setPaintLabels" -> null;
      default -> super.handle(proxy, method, args);
    };
  }
}
