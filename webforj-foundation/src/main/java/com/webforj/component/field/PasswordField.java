package com.webforj.component.field;

import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;

/**
 * The PasswordField provides a way for the user to securely enter a password.
 *
 * <p>
 * The element is presented as a one-line plain text editor control in which the text is obscured so
 * that it cannot be read, usually by replacing each character with a symbol such as the asterisk
 * ("*") or a dot ("•"). This character will vary depending on the user agent and operating system.
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
public final class PasswordField extends DwcTextField<PasswordField> {
  private boolean passwordReveal = true;

  /**
   * Constructs a new PasswordField with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public PasswordField(String label, String value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public PasswordField(String label, String value,
      EventListener<ValueChangeEvent<String>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public PasswordField(String label, String value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public PasswordField(String label, EventListener<ValueChangeEvent<String>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a value change listener.
   *
   * @param listener the value change listener
   */
  public PasswordField(EventListener<ValueChangeEvent<String>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a label.
   *
   * @param label the label of the field
   */
  public PasswordField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new PasswordField.
   */
  public PasswordField() {
    super();
    postInit();
  }

  private void postInit() { // NOSONAR
    setUnrestrictedProperty("type", "password");
  }

  /**
   * Shows or hides password reveal icon.
   *
   * @param passwordReveal when true, password reveal icon is visible, otherwise it is hidden.
   * @return the component
   */
  public PasswordField setPasswordReveal(boolean passwordReveal) {
    this.passwordReveal = passwordReveal;
    setUnrestrictedProperty("passwordReveal", passwordReveal);
    return this;
  }

  /**
   * Checks if password reveal icon is visible.
   *
   * @return true if password reveal icon is visible
   */
  public boolean isPasswordReveal() {
    return passwordReveal;
  }
}

