package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;

/**
 * Represents a transformer between a Double component value and a Float model value.
 *
 * <p>
 * A value that is not a number or is outside the Float range fails the transformation. A null value
 * stays null in both directions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class DoubleFloatTransformer implements Transformer<Double, Float> {
  private static final String NOT_A_NUMBER_MESSAGE = "The value must be a finite number.";
  private static final String RANGE_MESSAGE =
      "The value must be between -3.4028235E38 and 3.4028235E38.";

  /**
   * {@inheritDoc}
   */
  @Override
  public Float transformToModel(Double viewValue) {
    if (viewValue == null) {
      return null;
    }

    if (viewValue.isNaN()) {
      throw new TransformationException(NOT_A_NUMBER_MESSAGE);
    }

    float value = viewValue.floatValue();
    if (!Float.isFinite(value)) {
      throw new TransformationException(RANGE_MESSAGE);
    }

    return value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double transformToComponent(Float modelValue) {
    return modelValue == null ? null : Double.valueOf(modelValue.toString());
  }
}
