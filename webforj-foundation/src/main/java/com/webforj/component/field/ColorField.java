package com.webforj.component.field;

import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.awt.Color;

/**
 * The ColorField provides a user interface component that lets a user specify a color, either by
 * using a visual color picker interface or by entering the color into a text field in #rrggbb
 * hexadecimal format.
 *
 * <p>
 * Only simple colors (without alpha channel) are allowed and the component's presentation may vary
 * substantially from one browser and/or platform to another—it might be a simple textual field that
 * automatically validates to ensure that the color information is entered in the proper format, or
 * a platform-standard color picker, or some kind of custom color picker window.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class ColorField extends DwcFieldInitializer<ColorField, Color> {

  /**
   * Constructs a new color field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public ColorField(String label, Color value, EventListener<ValueChangeEvent<Color>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new color field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public ColorField(String label, EventListener<ValueChangeEvent<Color>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new color field with a value change listener.
   *
   * @param listener the value change listener
   */
  public ColorField(EventListener<ValueChangeEvent<Color>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new color field with a label.
   *
   * @param label the label of the field
   */
  public ColorField(String label) {
    super(label);
    postInit();
  }

  /**
   * Construct a new color field with the given value.
   *
   * @param value the value for the field
   */
  public ColorField(Color value) {
    super();
    setValue(value);
    postInit();
  }

  /**
   * Construct a new color field with the given label and value.
   *
   * @param label the label for the field
   * @param value the value for the field
   */
  public ColorField(String label, Color value) {
    super(label);
    setValue(value);
    postInit();
  }

  /**
   * Constructs a new color field.
   */
  public ColorField() {
    super();
    postInit();
  }

  private void postInit() {
    setUnrestrictedProperty("type", "color");
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalArgumentException if the given text is not a 7-character string hex color.
   */
  @Override
  public ColorField setText(String text) {
    if (text != null && !text.isEmpty() && !ColorField.isValidHexColor(text)) {
      throw new IllegalArgumentException(
          "Text must be a 7-character string" + " specifying an RGB color in hexadecimal format");
    }

    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ColorField setValue(Color value) {
    setText(value == null ? "" : ColorField.toHex(value));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Color getValue() {
    String value = getText();

    if (value == null || value.isEmpty()) {
      return Color.BLACK;
    }

    return ColorField.fromHex(value);
  }

  /**
   * Convert the given value to the corresponding color representation.
   *
   * @param hex The hex representation of the value to be converted
   * @return The corresponding color representation
   */
  public static Color fromHex(String hex) {
    return Color.decode(hex);
  }

  /**
   * Convert the given value to the corresponding hex representation.
   *
   * @param color The color representation of the value to be converted
   * @return The corresponding hex representation
   */
  public static String toHex(Color color) {
    return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
  }

  /**
   * Check if the given value is a valid 7 character hex color.
   *
   * @param hex The value to be checked
   * @return True if the value is a valid hex color, false otherwise
   */
  public static boolean isValidHexColor(String hex) {
    String hexColorPattern = "^#[0-9A-Fa-f]{6}$";
    return hex.matches(hexColorPattern);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Color convertValue(String value) {
    return ColorField.fromHex(value);
  }
}
