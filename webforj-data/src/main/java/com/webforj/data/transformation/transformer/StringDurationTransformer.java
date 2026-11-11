package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.time.Duration;
import java.time.format.DateTimeParseException;

/**
 * Represents a transformer between a String component value in the ISO 8601 duration format and a
 * Duration model value.
 *
 * <p>
 * Text that is not an ISO 8601 duration such as PT1H30M fails the transformation. Empty text
 * transforms to null and null transforms to null.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class StringDurationTransformer implements Transformer<String, Duration> {
  private static final String MESSAGE = "The value must be a duration such as PT1H30M.";

  /**
   * {@inheritDoc}
   */
  @Override
  public Duration transformToModel(String viewValue) {
    if (viewValue == null || viewValue.isBlank()) {
      return null;
    }

    try {
      return Duration.parse(viewValue.strip());
    } catch (DateTimeParseException e) {
      throw new TransformationException(MESSAGE, e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String transformToComponent(Duration modelValue) {
    return modelValue == null ? null : modelValue.toString();
  }
}
