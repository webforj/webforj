package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.util.UUID;

/**
 * Represents a transformer between a String component value and a UUID model value.
 *
 * <p>
 * Text that is not a UUID in its canonical form fails the transformation. Empty text transforms to
 * null and null transforms to null.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class StringUuidTransformer implements Transformer<String, UUID> {
  private static final String MESSAGE =
      "The value must be a UUID such as 123e4567-e89b-12d3-a456-426614174000.";

  /**
   * {@inheritDoc}
   */
  @Override
  public UUID transformToModel(String viewValue) {
    if (viewValue == null || viewValue.isBlank()) {
      return null;
    }

    String text = viewValue.strip();
    UUID uuid;
    try {
      uuid = UUID.fromString(text);
    } catch (IllegalArgumentException e) {
      throw new TransformationException(MESSAGE, e);
    }

    if (!uuid.toString().equalsIgnoreCase(text)) {
      throw new TransformationException(MESSAGE);
    }

    return uuid;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String transformToComponent(UUID modelValue) {
    return modelValue == null ? null : modelValue.toString();
  }
}
