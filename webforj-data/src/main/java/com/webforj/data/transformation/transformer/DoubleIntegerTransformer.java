package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.math.BigDecimal;

/**
 * Represents a transformer between a Double component value and an Integer model value.
 *
 * <p>
 * A value with a fraction or outside the Integer range fails the transformation. A null value stays
 * null in both directions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class DoubleIntegerTransformer implements Transformer<Double, Integer> {
  private static final String MESSAGE =
      "The value must be a whole number between -2147483648 and 2147483647.";

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer transformToModel(Double viewValue) {
    if (viewValue == null) {
      return null;
    }

    try {
      return new BigDecimal(viewValue).intValueExact();
    } catch (NumberFormatException | ArithmeticException e) {
      throw new TransformationException(MESSAGE, e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double transformToComponent(Integer modelValue) {
    return modelValue == null ? null : modelValue.doubleValue();
  }
}
