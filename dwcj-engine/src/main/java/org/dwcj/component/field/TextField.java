package org.dwcj.component.field;

import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.concern.HasPlaceholder;

/**
 * The TextField provides a user interface component to create a basic single-line text fields.
 *
 * <p>
 * The TextField can be configured to hint the browser to display a particular virtual keyboard for
 * numeric, email, phone number, or URL input and apply built-in validation to reject values that do
 * not adhere to the specified type.
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
public final class TextField extends DwcTextField<TextField> implements HasPlaceholder<TextField> {

  /**
   * Describes the type of the input field.
   */
  public enum Type {
    /**
     * A field for editing an email address. It has relevant keyboard in supporting browsers and
     * devices with dynamic keyboards.
     */
    EMAIL("email"),
    /**
     * A single-line text field for entering search strings. Line-breaks are automatically removed
     * from the input value.
     */
    SEARCH("search"),
    /**
     * A field for entering a telephone number. Displays a telephone keypad in some devices with
     * dynamic keypads.
     */
    TEL("tel"),
    /**
     * The default value. A single-line text field. Line-breaks are automatically removed from the
     * input value.
     */
    TEXT("text"),
    /**
     * A field for entering a URL. Looks it has relevant keyboard in supporting browsers and devices
     * with dynamic keyboards.
     */
    URL("url");

    private final String value;

    Type(String value) {
      this.value = value;
    }

    /**
     * Gets the value of the type.
     *
     * @return the value of the type.
     */
    public String getValue() {
      return value;
    }
  }

  private Type type;

  /**
   * Construct a new text field with the given type, label and value.
   *
   * @param type the type of field
   * @param label the label for the field
   * @param value the value for the field
   */
  public TextField(Type type, String label, String value) {
    super();

    setType(type);
    setLabel(label);
    setValue(value);
  }

  /**
   * Construct a new text field with the given type and label.
   *
   * @param type the type of field
   * @param label the label for the field
   */
  public TextField(Type type, String label) {
    this(type, label, "");
  }

  /**
   * Construct a new text field with the given type and empty value.
   *
   * @param type the type of field
   */
  public TextField(Type type) {
    this(type, "");
  }

  /**
   * Construct a new text field with the given label and value.
   *
   * @param label the label for the field
   * @param value the value for the field
   */
  public TextField(String label, String value) {
    this(Type.TEXT, label, value);
  }

  /**
   * Construct a new text field with the given label.
   *
   * @param label the label for the field
   */
  public TextField(String label) {
    this(label, "");
  }

  /**
   * Construct a new text field without any given data.
   */
  public TextField() {
    this("");
  }

  /**
   * Set the type of field.
   *
   * @param type the type of field
   * @return the field type
   */
  public TextField setType(Type type) {
    this.type = type;
    setUnrestrictedProperty("type", type.getValue());
    return this;
  }

  /**
   * Get the type of field.
   *
   * @return the type of field
   */
  public Type getType() {
    return type;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public TextField setPlaceholder(String placeholder) {
    return super.setComponentPlaceholder(placeholder);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getPlaceholder() {
    return super.getComponentPlaceholder();
  }
}
