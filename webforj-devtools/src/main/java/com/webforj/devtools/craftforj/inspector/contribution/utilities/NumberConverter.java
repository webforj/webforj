package com.webforj.devtools.craftforj.inspector.contribution.utilities;

/**
 * Utility for converting values to numeric types based on the current value's type.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class NumberConverter {

  private NumberConverter() {
    // Utility class
  }

  /**
   * Converts a value to the appropriate numeric type based on the current value's type.
   *
   * <p>
   * If currentValue is Integer, parses as Integer. If currentValue is Double, parses as Double.
   * Otherwise, infers type from string format (contains "." means Double).
   * </p>
   *
   * <p>
   * On parse failure, returns currentValue unchanged. If currentValue is null, null is returned
   * silently without error indication - callers should validate input if error reporting is needed.
   * </p>
   *
   * @param value the value to convert
   * @param currentValue the current value used to determine target type
   * @return the converted value, or currentValue if conversion fails
   */
  public static Object convert(Object value, Object currentValue) {
    if (value == null) {
      return null;
    }
    String strValue = value.toString();
    try {
      if (currentValue instanceof Integer) {
        return Integer.parseInt(strValue);
      } else if (currentValue instanceof Double) {
        return Double.parseDouble(strValue);
      } else if (strValue.contains(".")) {
        return Double.parseDouble(strValue);
      } else {
        return Integer.parseInt(strValue);
      }
    } catch (NumberFormatException e) {
      return currentValue;
    }
  }
}
