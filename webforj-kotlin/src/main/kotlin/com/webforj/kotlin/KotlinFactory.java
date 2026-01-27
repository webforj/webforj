package com.webforj.kotlin;

import com.webforj.component.field.TextField;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Utility class for use of <b>webforj</b> with <b>Kotlin</b>.
 * <p>
 * Provides static constructors for {@code sealed} component classes, so that they can be
 * initialized in <b>Kotlin</b>.
 *
 * @see TextField
 */
public final class KotlinFactory {

  public static TextField newTextField() {
    return new TextField();
  }

  public static TextField newTextField(String label) {
    return new TextField(label);
  }

  public static TextField newTextField(TextField.Type type) {
    return new TextField(type);
  }

  public static TextField newTextField(String label, String value) {
    return new TextField(label, value);
  }

  public static TextField newTextField(String label, String value, String placeholder) {
    return new TextField(label, value, placeholder);
  }

  public static TextField newTextField(String label, String value, TextField.Type type) {
    return new TextField(type, label, value);
  }

  public static TextField newTextField(String label, String value, String placeholder,
      TextField.Type type) {
    if (placeholder != null && value != null && label != null) {
      var tf = new TextField(label, value, placeholder);
      return type != null ? tf.setType(type) : tf;
    } else if (type != null && value != null && label != null) {
      return new TextField(type, label, value);
    } else if (value != null && label != null) {
      return new TextField(label, value);
    } else if (label != null) {
      var tf = new TextField(label);
      if (placeholder != null)
        tf.setPlaceholder(placeholder);
      return type != null ? tf.setType(type) : tf;
    } else if (type != null) {
      var tf = new TextField(type);
      if (value != null)
        tf.setValue(value);
      return placeholder != null ? tf.setPlaceholder(placeholder) : tf;
    } else {
      var tf = new TextField();
      if (value != null)
        tf.setValue(value);
      return placeholder != null ? tf.setPlaceholder(placeholder) : tf;
    }
  }

  private KotlinFactory() {
    throw new WebforjRuntimeException("No instance");
  }

}
