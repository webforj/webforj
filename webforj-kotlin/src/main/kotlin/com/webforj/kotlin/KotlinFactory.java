package com.webforj.kotlin;

import com.webforj.component.field.TextField;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Utility class for use of <b>webforj</b> with <b>Kotlin</b>.
 *
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

  private KotlinFactory() {
    throw new WebforjRuntimeException("No instance");
  }

}
