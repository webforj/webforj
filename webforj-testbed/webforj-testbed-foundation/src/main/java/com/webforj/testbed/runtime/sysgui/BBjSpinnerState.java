package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.function.Consumer;

/**
 * The spinning part shared by the spinner mocks.
 *
 * <p>
 * A spinner with a list steps through the list and shows the entry. A spinner without one steps the
 * value of its control, which the owning mock supplies.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjSpinnerState {
  static final Object UNHANDLED_SENTINEL = new Object();
  private final Consumer<String> textSink;
  private final Stepper valueStepper;
  private BBjVector spinList = new BBjVector();
  private int listIndex = 0;
  private int spinField = 0;

  BBjSpinnerState(Consumer<String> textSink, Stepper valueStepper) {
    this.textSink = textSink;
    this.valueStepper = valueStepper;
  }

  int getSpinField() {
    return spinField;
  }

  int indexOf(String text) {
    for (int index = 0; index < spinList.size(); index++) {
      if (String.valueOf(spinList.get(index)).equals(text)) {
        return index;
      }
    }

    return -1;
  }

  void setListIndexSilently(int index) {
    listIndex = index;
  }

  Object handle(String methodName, Object[] args) throws BBjException {
    return switch (methodName) {
      case "spin" -> {
        boolean up = Boolean.TRUE.equals(args[0]);
        if (spinList.isEmpty()) {
          if (valueStepper != null) {
            valueStepper.step(up);
          }
        } else {
          listIndex = Math.clamp((long) listIndex + (up ? 1 : -1), 0, spinList.size() - 1);
          textSink.accept(String.valueOf(spinList.get(listIndex)));
        }
        yield null;
      }
      case "setSpinList" -> {
        if (args[0] == null) {
          throw new BBjException("The spin list cannot be null", BBjException.ARG_ERR);
        }
        spinList = (BBjVector) args[0];
        yield null;
      }
      case "getSpinList" -> spinList;
      case "setListIndex" -> {
        int index = (Integer) args[0];
        if (index < 0 || index >= spinList.size()) {
          throw new BBjException("Index out of range for list: " + index, BBjException.ARG_ERR);
        }
        listIndex = index;
        textSink.accept(String.valueOf(spinList.get(listIndex)));
        yield null;
      }
      case "getListIndex" -> listIndex;
      case "setSpinField" -> {
        spinField = (Integer) args[0];
        yield null;
      }
      case "getSpinField" -> spinField;
      default -> UNHANDLED_SENTINEL;
    };
  }

  /**
   * Steps the value of a spinner that has no list.
   */
  interface Stepper {
    void step(boolean up) throws BBjException;
  }
}
