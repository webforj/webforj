package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import java.lang.reflect.Method;

/**
 * The mock backing the masked number input control.
 *
 * <p>
 * Driven by {@code com.webforj.component.field.MaskedNumberField}, which writes the group
 * character, the decimal character and the negateable flag through the control.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInputNMock extends BBjInputMock {
  private String commaCharacter = ",";
  private String dotCharacter = ".";
  private boolean negateable = true;

  /**
   * Creates a new masked number input mock.
   *
   * @param id the control identifier
   */
  BBjInputNMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setValue" -> {
        putText(BasisNumber.asBasisNumber((BBjNumber) args[0]).formatStringWithGivenPrecision(16));
        yield null;
      }
      case "getValue" -> getNumberValue();
      case "setCommaCharacter" -> {
        commaCharacter = (String) args[0];
        yield null;
      }
      case "getCommaCharacter" -> commaCharacter;
      case "setDotCharacter" -> {
        dotCharacter = (String) args[0];
        yield null;
      }
      case "getDotCharacter" -> dotCharacter;
      case "setNegateable" -> {
        negateable = (Boolean) args[0];
        yield null;
      }
      case "getNegateable" -> negateable;
      default -> super.handle(proxy, method, args);
    };
  }

  /**
   * Returns the number the text of the control reads as, the way the runtime reads it.
   *
   * @return the number, zero for an empty text or a lone decimal point
   * @throws BBjException if the text is not a number
   */
  BBjNumber getNumberValue() throws BBjException {
    Object stored = getState("text");
    String text = stored == null ? "" : stored.toString();
    text = text.equals(".") ? "" : text.trim();
    if (text.isEmpty()) {
      return BasisNumber.ZERO;
    }

    try {
      return new BasisNumber(text);
    } catch (NumberFormatException e) {
      throw new BBjException(e, BBjException.TYPE_ERR);
    }
  }
}
