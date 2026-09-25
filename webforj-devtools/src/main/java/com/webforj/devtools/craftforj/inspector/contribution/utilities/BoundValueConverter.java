package com.webforj.devtools.craftforj.inspector.contribution.utilities;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/** Converts minimum and maximum text using the component's declared bound type. */
public final class BoundValueConverter {

  private BoundValueConverter() {}

  /**
   * Converts a bound, including temporal bounds whose current value is null.
   *
   * @param component the component implementing the bound getter
   * @param getterName the public getter declaring the bound type
   * @param value the value supplied by the editor
   * @param currentValue the current bound used for numeric conversion
   * @return the typed bound, or null when cleared
   */
  public static Object convert(Object component, String getterName, Object value,
      Object currentValue) {
    if (value == null || "".equals(value)) {
      return null;
    }

    try {
      Class<?> type = component.getClass().getMethod(getterName).getReturnType();
      String text = value.toString();
      return switch (type.getName()) {
        case "java.time.LocalDate" -> LocalDate.parse(text);
        case "java.time.LocalTime" -> LocalTime.parse(text);
        case "java.time.LocalDateTime" -> LocalDateTime.parse(text);
        default -> NumberConverter.convert(value, currentValue);
      };
    } catch (NoSuchMethodException e) {
      throw new IllegalArgumentException("Bound getter not found: " + getterName, e);
    }
  }
}
