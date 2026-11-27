package com.webforj.data.transformation.transformer;

/**
 * Represents a transformer between a String component value and a String model value that has no
 * value when the text is blank.
 *
 * <p>
 * Blank text transforms to null and any other text transforms to itself. Null transforms to empty
 * text.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class BlankStringTransformer implements Transformer<String, String> {

  /**
   * {@inheritDoc}
   */
  @Override
  public String transformToModel(String viewValue) {
    return viewValue == null || viewValue.isBlank() ? null : viewValue;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String transformToComponent(String modelValue) {
    return modelValue == null ? "" : modelValue;
  }
}
