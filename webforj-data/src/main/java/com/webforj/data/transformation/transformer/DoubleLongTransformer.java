package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.math.BigDecimal;

/**
 * Represents a transformer between a Double component value and a Long model value.
 *
 * <p>
 * A value with a fraction or outside the Long range fails the transformation. A null value stays
 * null in both directions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class DoubleLongTransformer implements Transformer<Double, Long> {
  private static final String MESSAGE =
      "The value must be a whole number between -9223372036854775808 and 9223372036854775807.";

  /**
   * {@inheritDoc}
   */
  @Override
  public Long transformToModel(Double viewValue) {
    if (viewValue == null) {
      return null;
    }

    try {
      return new BigDecimal(viewValue).longValueExact();
    } catch (NumberFormatException | ArithmeticException e) {
      throw new TransformationException(MESSAGE, e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double transformToComponent(Long modelValue) {
    return modelValue == null ? null : modelValue.doubleValue();
  }
}
