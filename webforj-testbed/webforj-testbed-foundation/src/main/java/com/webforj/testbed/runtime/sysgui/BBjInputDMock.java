package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import com.webforj.MaskDecorator;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import java.lang.reflect.Method;
import java.time.LocalDate;

/**
 * The mock backing the masked date input.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputDMock extends BBjInputMock {
  private static final String DEFAULT_MASK = "%Mz/%Dz/%Yl";
  private final JulianLocaleDateTransformer julian = new JulianLocaleDateTransformer();
  // The runtime reports -1 for an empty field, which the transformer reads as no date.
  private BasisNumber value = BasisNumber.valueOf(-1);

  BBjInputDMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setValue" -> {
        value = BasisNumber.valueOf(((BBjNumber) args[0]).intValue());
        LocalDate date = value.intValue() < 0 ? null : julian.transformToModel(value.intValue());
        putState("text", date == null ? "" : MaskDecorator.forDate(date, mask()));
        yield null;
      }
      case "getValue" -> value;
      case "setText" -> {
        String text = args[0] == null ? "" : (String) args[0];
        putState("text", text);
        LocalDate date = text.isEmpty() ? null : MaskDecorator.parseDate(text, mask());
        value = BasisNumber.valueOf(date == null ? -1 : julian.transformToComponent(date));
        yield null;
      }
      case "setShowWeeks", "calendar" -> null;
      default -> super.handle(proxy, method, args);
    };
  }

  LocalDate getDate() {
    return value.intValue() < 0 ? null : julian.transformToModel(value.intValue());
  }

  void setDate(LocalDate date) {
    value = BasisNumber.valueOf(date == null ? -1 : julian.transformToComponent(date));
    putState("text", date == null ? "" : MaskDecorator.forDate(date, mask()));
  }

  private String mask() {
    return getMask().isEmpty() ? DEFAULT_MASK : getMask();
  }
}
