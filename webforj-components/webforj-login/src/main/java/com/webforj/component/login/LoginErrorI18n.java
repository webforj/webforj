package com.webforj.component.login;

/**
 * The login dialog translation object.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public final class LoginErrorI18n {
  private String title = "Incorrect username or password";
  private String message =
      "Check that you have entered the correct username and password and try again.";

  /**
   * Gets the error title.
   *
   * @return the error title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets the error title.
   *
   * @param title the error title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Gets the error message.
   *
   * @return the error message
   */
  public String getMessage() {
    return message;
  }

  /**
   * Sets the error message.
   *
   * @param message the error message
   */
  public void setMessage(String message) {
    this.message = message;
  }
}
