package com.webforj;

import com.basis.util.BasisDate;
import com.basis.util.BasisFunctions;
import com.webforj.data.transformation.transformer.HoursLocalTimeTransformer;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Locale;
import java.util.Objects;

/**
 * Utility class for decorating or modifying an input with a webforJ mask.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public final class MaskDecorator {
  static final String MASK_CANNOT_BE_NULL = "The mask cannot be null";
  static final String INPUT_CANNOT_BE_NULL = "The input cannot be null";

  private MaskDecorator() {}

  /**
   * Masks the given input string with the given mask.
   *
   * <p>
   * The mask format is as follows:
   * </p>
   *
   * <table border="1">
   * <caption>Mask Format</caption>
   * <tr>
   * <th>Mask Character</th>
   * <th>Accepts</th>
   * </tr>
   * <tr>
   * <td>X</td>
   * <td>Any printable character.</td>
   * </tr>
   * <tr>
   * <td>a</td>
   * <td>Any alphabetic character.</td>
   * </tr>
   * <tr>
   * <td>A</td>
   * <td>Any alphabetic character. Converts lower-case alphabetic characters to upper case.</td>
   * </tr>
   * <tr>
   * <td>0</td>
   * <td>Any digit.</td>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Any digit or alphabetic character.</td>
   * </tr>
   * <tr>
   * <td>Z</td>
   * <td>Any digit or alphabetic character. Converts lower-case alphabetic characters to upper
   * case.</td>
   * </tr>
   * </table>
   *
   * <p>
   * Any other character represents itself.
   * </p>
   *
   * @param input the input string
   * @param mask the mask
   *
   * @return the masked string
   */
  public static String forString(String input, String mask) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      return BasisFunctions.str(input, mask);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Masks the given input number with the given mask.
   *
   * <p>
   * The mask format is as follows:
   * </p>
   *
   * <table border="1">
   * <tr>
   * <th>Character</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>0</td>
   * <td>A zero is always replaced by a digit (0..9).</td>
   * </tr>
   * <tr>
   * <td>#</td>
   * <td>The pound sign is used to suppress leading zeroes. It is replaced by the fill character for
   * leading zeroes to the left of the decimal point. For trailing zeros to the right of the decimal
   * point, it is replaced by a space or a zero. Any other time it is replaced by a digit.</td>
   * </tr>
   * <tr>
   * <td>,</td>
   * <td>To the left of the decimal point, the comma is replaced by the fill character if no digits
   * have yet been placed. Any other time, it results in a comma.</td>
   * </tr>
   * <tr>
   * <td>-</td>
   * <td>The minus sign creates a "-" in the result if the number is negative; otherwise, it is
   * replaced by the fill character.</td>
   * </tr>
   * <tr>
   * <td>+</td>
   * <td>The plus sign becomes a "+" in the result if the number is positive, or a "-" if the number
   * is negative.</td>
   * </tr>
   * <tr>
   * <td>$</td>
   * <td>The dollar sign always results in a dollar sign.</td>
   * </tr>
   * <tr>
   * <td>(</td>
   * <td>A left parenthesis results in a "(" if the number is negative, or the fill character if
   * positive.</td>
   * </tr>
   * <tr>
   * <td>)</td>
   * <td>A right parenthesis results in a ")" if the number is negative, or the fill character if
   * positive.</td>
   * </tr>
   * <tr>
   * <td>CR</td>
   * <td>The characters "CR" are inserted into the number if the number is negative. Two spaces are
   * inserted if the number is positive.</td>
   * </tr>
   * <tr>
   * <td>DR</td>
   * <td>The characters "CR" are inserted into the number if the number is negative. The characters
   * "DR" are inserted if the number is positive.</td>
   * </tr>
   * <tr>
   * <td>*</td>
   * <td>The asterisk "*" is inserted into the number.</td>
   * </tr>
   * <tr>
   * <td>.</td>
   * <td>The decimal point is replaced by a decimal point if any digits appear in the output mask.
   * Otherwise, it is replaced by the fill character. After the decimal point, the fill character
   * becomes a space.</td>
   * </tr>
   * <tr>
   * <td>B</td>
   * <td>The upper case "B" always becomes a space. Any other character is simply copied to the
   * result.</td>
   * </tr>
   * </table>
   *
   * <p>
   * Some of the above characters may possibly float within the mask. These are "-", "+", "$", and
   * "(". If any of these characters is present in the mask, the first one encountered will be moved
   * to the last position where a "#" or "," was replaced by the fill character. If no such position
   * exists, the float character is left where it is.
   * </p>
   *
   * @param input the input number
   * @param mask the mask
   *
   * @return the masked number
   */
  public static String forNumber(double input, String mask) {
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      return BasisFunctions.str(input, mask);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Masks the given input date with the given mask.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the date to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%Y</td>
   * <td>Year</td>
   * </tr>
   * <tr>
   * <td>%M</td>
   * <td>Month</td>
   * </tr>
   * <tr>
   * <td>%D</td>
   * <td>Day</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input date
   * @param mask the mask
   *
   * @return the masked date
   */
  public static String forDate(LocalDate input, String mask) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      JulianLocaleDateTransformer transformer = new JulianLocaleDateTransformer();
      int julian = transformer.transformToComponent(input);
      return BasisDate.date(julian, 0.0, mask);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Masks the given input date with the given mask.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the time to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%H</td>
   * <td>Hour (24-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%h</td>
   * <td>Hour (12-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%m</td>
   * <td>Minute</td>
   * </tr>
   * <tr>
   * <td>%s</td>
   * <td>Second</td>
   * </tr>
   * <tr>
   * <td>%p</td>
   * <td>AM/PM</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input time
   * @param mask the mask
   *
   * @return the masked time
   */
  public static String forTime(LocalTime input, String mask) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      HoursLocalTimeTransformer transformer = new HoursLocalTimeTransformer();
      double hms = transformer.transformToComponent(input);
      return BasisDate.date(0, hms, mask);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Masks the given input date and time with the given mask.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the date/time to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%Y</td>
   * <td>Year</td>
   * </tr>
   * <tr>
   * <td>%M</td>
   * <td>Month</td>
   * </tr>
   * <tr>
   * <td>%D</td>
   * <td>Day</td>
   * </tr>
   * <tr>
   * <td>%H</td>
   * <td>Hour (24-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%h</td>
   * <td>Hour (12-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%m</td>
   * <td>Minute</td>
   * </tr>
   * <tr>
   * <td>%s</td>
   * <td>Second</td>
   * </tr>
   * <tr>
   * <td>%p</td>
   * <td>AM/PM</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input date
   * @param mask the mask
   *
   * @return the masked date and time
   */
  public static String forDateTime(LocalDateTime input, String mask) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      JulianLocaleDateTransformer dateTransformer = new JulianLocaleDateTransformer();
      HoursLocalTimeTransformer timeTransformer = new HoursLocalTimeTransformer();
      int julian = dateTransformer.transformToComponent(input.toLocalDate());
      double hms = timeTransformer.transformToComponent(input.toLocalTime());
      return BasisDate.date(julian, hms, mask);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Parse a masked date string using the given mask and locale.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the date to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%Y</td>
   * <td>Year</td>
   * </tr>
   * <tr>
   * <td>%M</td>
   * <td>Month</td>
   * </tr>
   * <tr>
   * <td>%D</td>
   * <td>Day</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input date
   * @param mask the mask
   * @param locale the locale to use for parsing the date. The locale is only relevant when passing
   *        a date string containing a week number reference
   *
   * @return a LocalDate object
   */
  public static LocalDate parseDate(String input, String mask, Locale locale) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      Locale effectiveLocale = locale == null ? App.getLocale() : locale;
      int julian = BasisDate.parse(input, mask, effectiveLocale, true, 50);
      JulianLocaleDateTransformer transformer = new JulianLocaleDateTransformer();
      return transformer.transformToModel(julian);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Parse a masked date string using the given mask and default locale.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the date to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%Y</td>
   * <td>Year</td>
   * </tr>
   * <tr>
   * <td>%M</td>
   * <td>Month</td>
   * </tr>
   * <tr>
   * <td>%D</td>
   * <td>Day</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input date
   * @param mask the mask
   *
   * @return a LocalDate object
   * @see #parseDate(String, String, Locale)
   */
  public static LocalDate parseDate(String input, String mask) {
    return parseDate(input, mask, null);
  }

  /**
   * Parse the given input time with the given mask and locale.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the time to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%H</td>
   * <td>Hour (24-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%h</td>
   * <td>Hour (12-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%m</td>
   * <td>Minute</td>
   * </tr>
   * <tr>
   * <td>%s</td>
   * <td>Second</td>
   * </tr>
   * <tr>
   * <td>%p</td>
   * <td>AM/PM</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input time
   * @param mask the mask
   * @param locale the locale to use for parsing the time. The locale is only relevant when passing
   *        a time string containing a localized am/pm value
   *
   * @return a LocalTime object
   */
  public static LocalTime parseTime(String input, String mask, Locale locale) {
    Objects.requireNonNull(input, INPUT_CANNOT_BE_NULL);
    Objects.requireNonNull(mask, MASK_CANNOT_BE_NULL);

    try {
      Locale effectiveLocale = locale == null ? App.getLocale() : locale;
      double hms = BasisDate.parse(input, mask, effectiveLocale);
      HoursLocalTimeTransformer transformer = new HoursLocalTimeTransformer();
      return transformer.transformToModel(hms);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Parse the given input time with the given mask and default locale.
   *
   * <p>
   * webforJ recognizes several format indicators that all begin with a "%", followed by a letter
   * indicating which component of the time to insert:
   * </p>
   *
   * <table border="1">
   * <caption>Format Indicators</caption>
   * <tr>
   * <th>Format</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>%H</td>
   * <td>Hour (24-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%h</td>
   * <td>Hour (12-hour clock)</td>
   * </tr>
   * <tr>
   * <td>%m</td>
   * <td>Minute</td>
   * </tr>
   * <tr>
   * <td>%s</td>
   * <td>Second</td>
   * </tr>
   * <tr>
   * <td>%p</td>
   * <td>AM/PM</td>
   * </tr>
   * </table>
   *
   * <p>
   * An optional modifier can follow format indicators to describe more specific information:
   * </p>
   *
   * <table border="1">
   * <caption>Modifiers</caption>
   * <tr>
   * <th>Modifier</th>
   * <th>Description</th>
   * </tr>
   * <tr>
   * <td>z</td>
   * <td>Zero-fill</td>
   * </tr>
   * <tr>
   * <td>s</td>
   * <td>Short text</td>
   * </tr>
   * <tr>
   * <td>l</td>
   * <td>Long text</td>
   * </tr>
   * <tr>
   * <td>p</td>
   * <td>Packed number</td>
   * </tr>
   * <tr>
   * <td>d</td>
   * <td>Decimal (default format)</td>
   * </tr>
   * </table>
   *
   * @param input the input time
   * @param mask the mask
   *
   * @return a LocalTime object
   */
  public static LocalTime parseTime(String input, String mask) {
    return parseTime(input, mask, null);
  }
}
