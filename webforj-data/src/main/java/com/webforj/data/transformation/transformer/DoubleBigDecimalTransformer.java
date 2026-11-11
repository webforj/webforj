package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.math.BigDecimal;

/**
 * Represents a transformer between a Double component value and a BigDecimal model value.
 *
 * <p>
 * The model value holds the shortest decimal digits of the number without trailing zeros and with a
 * scale of at least 0, so 10000000.0 transforms to 10000000 and 1234.5 to 1234.5. A value that is
 * not a finite number fails the transformation. A null value stays null in both directions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class DoubleBigDecimalTransformer implements Transformer<Double, BigDecimal> {
  private static final String MESSAGE = "The value must be a finite number.";

  /**
   * {@inheritDoc}
   */
  @Override
  public BigDecimal transformToModel(Double viewValue) {
    if (viewValue == null) {
      return null;
    }

    BigDecimal value;
    try {
      value = BigDecimal.valueOf(viewValue).stripTrailingZeros();
    } catch (NumberFormatException e) {
      throw new TransformationException(MESSAGE, e);
    }

    return value.scale() < 0 ? value.setScale(0) : value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double transformToComponent(BigDecimal modelValue) {
    return modelValue == null ? null : modelValue.doubleValue();
  }
}
