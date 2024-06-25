package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.time.LocalDate;

/**
 * Represents a transformer value between the Julian date and java LocalDate.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public class JulianLocaleDateTransformer implements Transformer<Integer, LocalDate> {

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDate transformToModel(Integer viewValue) {
    try {
      if (viewValue < 0) {
        return null;
      }

      if (viewValue == 0) {
        return LocalDate.now();
      }

      int n = viewValue - 1721425;
      int year = n / 365;

      int temp;
      while ((temp = year * 365 + year / 4 - year / 100 + year / 400) >= n) {
        --year;
      }

      n -= temp;
      ++year;

      if ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0))) {
        if (n > 60) {
          ++n;
        }
      } else {
        if (n > 59) {
          n += 2;
        }
      }

      n += 91;
      int month = (n * 100) / 3055;
      int day = n - (month * 3055) / 100;
      month -= 2;

      return LocalDate.of(year, month, day);
    } catch (Exception e) {
      throw new TransformationException("Error transforming component value to model value");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer transformToComponent(LocalDate modelValue) {
    try {
      if (modelValue == null) {
        return -1;
      }

      int year = modelValue.getYear();
      int month = modelValue.getMonthValue();
      int day = modelValue.getDayOfMonth();

      boolean leap = isLeapYear(year);

      int num = year - 1;
      num = (num * 365) + (num / 4) - (num / 100) + (num / 400);

      day += ((3055 * (month + 2)) / 100) - 91;
      if (month > 2) {
        if (leap) {
          day--;
        } else {
          day -= 2;
        }
      }

      return num + day + 1721425;
    } catch (Exception e) {
      throw new TransformationException("Error transforming model value to component value");
    }
  }

  private static boolean isLeapYear(int year) {
    return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
  }
}
