package com.webforj.kotlin;

import com.webforj.component.field.MaskedDateField;
import com.webforj.component.field.MaskedNumberField;
import com.webforj.component.field.MaskedTextField;
import com.webforj.component.field.MaskedTimeField;
import com.webforj.component.field.TextField;
import com.webforj.exceptions.WebforjRuntimeException;

import java.time.LocalDate;
import java.time.LocalTime;

/**
 * Utility class for use of <b>webforj</b> with <b>Kotlin</b>.
 *
 * <p>Provides static constructors for {@code sealed} component classes, so that they can
 * be initialized in <b>Kotlin</b>.
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
      if (placeholder != null) {
        tf.setPlaceholder(placeholder);
      }
      return type != null ? tf.setType(type) : tf;
    } else if (type != null) {
      var tf = new TextField(type);
      if (value != null) {
        tf.setValue(value);
      }
      return placeholder != null ? tf.setPlaceholder(placeholder) : tf;
    } else {
      var tf = new TextField();
      if (value != null) {
        tf.setValue(value);
      }
      return placeholder != null ? tf.setPlaceholder(placeholder) : tf;
    }
  }

  public static MaskedDateField newMaskedDateField() {
    return new MaskedDateField();
  }

  public static MaskedDateField newMaskedDateField(String label) {
    return new MaskedDateField(label);
  }

  public static MaskedDateField newMaskedDateField(String label, LocalDate value) {
    return new MaskedDateField(label, value);
  }

  public static MaskedDateField newMaskedDateField(
      String label, LocalDate value, String placeholder) {
    return new MaskedDateField(label, value, placeholder);
  }

  public static MaskedTimeField newMaskedTimeField() {
    return new MaskedTimeField();
  }

  public static MaskedTimeField newMaskedTimeField(String label) {
    return new MaskedTimeField(label);
  }

  public static MaskedTimeField newMaskedTimeField(String label, LocalTime value) {
    return new MaskedTimeField(label, value);
  }

  public static MaskedTimeField newMaskedTimeField(
      String label, LocalTime value, String placeholder) {
    return new MaskedTimeField(label, value, placeholder);
  }

  public static MaskedNumberField newMaskedNumberField() {
    return new MaskedNumberField();
  }

  public static MaskedNumberField newMaskedNumberField(String label) {
    return new MaskedNumberField(label);
  }

  public static MaskedNumberField newMaskedNumberField(String label, Double value) {
    return new MaskedNumberField(label, value);
  }

  public static MaskedNumberField newMaskedNumberField(
      String label, Double value, String placeholder) {
    return new MaskedNumberField(label, value, placeholder);
  }

  public static MaskedTextField newMaskedTextField() {
    return new MaskedTextField();
  }

  public static MaskedTextField newMaskedTextField(String label) {
    return new MaskedTextField(label);
  }

  public static MaskedTextField newMaskedTextField(String label, String value) {
    return new MaskedTextField(label, value);
  }

  public static MaskedTextField newMaskedTextField(String label, String value, String placeholder) {
    return new MaskedTextField(label, value, placeholder);
  }

  private KotlinFactory() {
    throw new WebforjRuntimeException("No instance");
  }

}
