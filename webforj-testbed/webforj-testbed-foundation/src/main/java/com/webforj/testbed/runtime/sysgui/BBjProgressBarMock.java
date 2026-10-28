package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import java.lang.reflect.Method;

/**
 * The mock backing the progress bar control.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjProgressBarMock extends BBjControlMock {
  private static final int HORIZONTAL = 0;
  private static final int VERTICAL = 1;
  private int value = 0;
  private int minimum = 0;
  private int maximum = 100;

  BBjProgressBarMock(int id) {
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
      case "setOrientation" -> {
        int orientation = (Integer) args[0];
        if (orientation != HORIZONTAL && orientation != VERTICAL) {
          throw new BBjException("Invalid orientation " + orientation, BBjException.ARG_ERR);
        }
        putState("orientation", orientation);
        yield null;
      }
      case "setIndeterminate", "setStringPainted" -> null;
      default -> super.handle(proxy, method, args);
    };
  }
}
