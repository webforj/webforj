package org.dwcj.component.field;

import org.dwcj.concern.HasPlaceholder;

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
public final class PasswordField extends AbstractDwcTextField<PasswordField>
    implements HasPlaceholder<PasswordField> {

  private boolean passwordReveal = true;
  private String placeholder = null;


  /**
   * Construct a new password field with the given label and password.
   *
   * @param label the label for the field
   * @param password the value for the field
   */
  public PasswordField(String label, String password) {
    super();

    setUnrestrictedProperty("type", "password");
    setLabel(label);
    setValue(password);
  }

  /**
   * Construct a new password field with the given label.
   *
   * @param label the label for the field
   */
  public PasswordField(String label) {
    this(label, "");
  }

  /**
   * Construct a new password field.
   */
  public PasswordField() {
    this("");
  }

  /**
   * Show or hide password reveal icon.
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
   * Check if password reveal icon is visible.
   *
   * @return true if password reveal icon is visible
   */
  public boolean isPasswordReveal() {
    return passwordReveal;
  }

  /**
   * Set the placeholder of field.
   *
   * @param placeholder the placeholder of field
   * @return the field type
   */
  public PasswordField setPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    setUnrestrictedProperty("placeholder", placeholder);
    return this;
  }

  /**
   * Get the placeholder of field.
   *
   * @return the placeholder of field
   */
  public String getPlaceholder() {
    return placeholder;
  }
}

