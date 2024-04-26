package com.webforj.component.login;

import com.google.gson.annotations.SerializedName;

/**
 * The login dialog translation object.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public final class LoginI18n {
  @SerializedName("title")
  private String title = "Authentication";
  @SerializedName("usernamePrompt")
  private String username = "Username";
  @SerializedName("passwordPrompt")
  private String password = "Password";
  @SerializedName("rememberMe")
  private String rememberMe = "Remember me";
  @SerializedName("loginButton")
  private String submit = "Sign in";
  @SerializedName("cancelButton")
  private String cancel = "";
  private LoginErrorI18n error = new LoginErrorI18n();

  /**
   * Gets the dialog title.
   *
   * @return the dialog title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets the dialog title.
   *
   * @param title the dialog title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Gets the username prompt.
   *
   * @return the username prompt
   */
  public String getUsername() {
    return username;
  }

  /**
   * Sets the username prompt.
   *
   * @param usernamePrompt the username prompt
   */
  public void setUsername(String usernamePrompt) {
    this.username = usernamePrompt;
  }

  /**
   * Gets the password prompt.
   *
   * @return the password prompt
   */
  public String getPassword() {
    return password;
  }

  /**
   * Sets the password prompt.
   *
   * @param passwordPrompt the password prompt
   */
  public void setPassword(String passwordPrompt) {
    this.password = passwordPrompt;
  }

  /**
   * Gets the remember me text.
   *
   * @return the remember me text
   */
  public String getRememberMe() {
    return rememberMe;
  }

  /**
   * Sets the remember me text.
   *
   * @param rememberMe the remember me text
   */
  public void setRememberMe(String rememberMe) {
    this.rememberMe = rememberMe;
  }

  /**
   * Gets the login button text.
   *
   * @return the login button text
   */
  public String getSubmit() {
    return submit;
  }

  /**
   * Sets the login button text.
   *
   * @param loginButton the login button text
   */
  public void setSubmit(String loginButton) {
    this.submit = loginButton;
  }

  /**
   * Gets the cancel button text.
   *
   * @return the cancel button text
   */
  public String getCancel() {
    return cancel;
  }

  /**
   * Sets the cancel button text.
   *
   * @param cancelButton the cancel button text
   */
  public void setCancel(String cancelButton) {
    this.cancel = cancelButton;
  }

  /**
   * Gets the error I18n object.
   *
   * @return the error I18n object
   */
  public LoginErrorI18n getError() {
    return error;
  }
}
