package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import com.webforj.MaskDecorator;
import com.webforj.data.transformation.transformer.HoursLocalTimeTransformer;
import java.lang.reflect.Method;
import java.time.LocalTime;

/**
 * The mock backing the masked time input.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputTMock extends BBjInputMock {
  private static final String DEFAULT_MASK = "%h:%mz %a";
  private final HoursLocalTimeTransformer hours = new HoursLocalTimeTransformer();
  // The runtime reports -1 for an empty field, which the transformer reads as no time.
  private static final BasisNumber EMPTY = BasisNumber.valueOf(-1);
  private BasisNumber value = EMPTY;

  BBjInputTMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setValue" -> {
        double hoursOfDay = ((BBjNumber) args[0]).doubleValue();
        setTime(hoursOfDay < 0 ? null : hours.transformToModel(hoursOfDay));
        yield null;
      }
      case "getValue" -> value;
      case "setText" -> {
        String text = args[0] == null ? "" : (String) args[0];
        putState("text", text);
        LocalTime time = text.isEmpty() ? null : MaskDecorator.parseTime(text, mask());
        value = time == null ? EMPTY : BasisNumber.valueOf(hours.transformToComponent(time));
        yield null;
      }
      case "showTimePicker" -> null;
      default -> super.handle(proxy, method, args);
    };
  }

  LocalTime getTime() {
    return value.doubleValue() < 0 ? null : hours.transformToModel(value.doubleValue());
  }

  void setTime(LocalTime time) {
    value = time == null ? EMPTY : BasisNumber.valueOf(hours.transformToComponent(time));
    putState("text", time == null ? "" : MaskDecorator.forTime(time, mask()));
  }

  private String mask() {
    return getMask().isEmpty() ? DEFAULT_MASK : getMask();
  }
}
