package com.webforj.devtools.craftforj.inspector.contribution.utilities;

import java.util.function.Consumer;
import java.util.function.ObjDoubleConsumer;
import java.util.regex.Pattern;

/**
 * Utility for setting dimension values intelligently.
 *
 * <p>
 * Determines whether to use the float setter (for pure numbers) or string setter (for CSS values
 * with units like "px", "%", "em", "auto", etc.).
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class DimensionSetter {

  private static final Pattern PURE_NUMBER = Pattern.compile("^-?\\d+(\\.\\d+)?$");

  private DimensionSetter() {}

  /**
   * Sets a dimension value using the appropriate setter.
   *
   * @param <T> the component type
   * @param component the component
   * @param value the value (e.g., "500", "500px", "50%", "auto")
   * @param floatSetter setter for float values (adds "px" automatically)
   * @param stringSetter setter for string values (used as-is)
   */
  public static <T> void set(T component, Object value, ObjDoubleConsumer<T> floatSetter,
      Consumer<String> stringSetter) {
    if (value == null) {
      stringSetter.accept("");
      return;
    }

    String strValue = String.valueOf(value).trim();
    if (strValue.isEmpty()) {
      stringSetter.accept("");
      return;
    }

    if (PURE_NUMBER.matcher(strValue).matches()) {
      floatSetter.accept(component, Double.parseDouble(strValue));
    } else {
      stringSetter.accept(strValue);
    }
  }
}
