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

  /**
   * Get the dialog title.
   *
   * @return the dialog title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Set the dialog title.
   *
   * @param title the dialog title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Get the username prompt.
   *
   * @return the username prompt
   */
  public String getUsername() {
    return username;
  }

  /**
   * Set the username prompt.
   *
   * @param usernamePrompt the username prompt
   */
  public void setUsername(String usernamePrompt) {
    this.username = usernamePrompt;
  }

  /**
   * Get the password prompt.
   *
   * @return the password prompt
   */
  public String getPassword() {
    return password;
  }

  /**
   * Set the password prompt.
   *
   * @param passwordPrompt the password prompt
   */
  public void setPassword(String passwordPrompt) {
    this.password = passwordPrompt;
  }

  /**
   * Get the remember me text.
   *
   * @return the remember me text
   */
  public String getRememberMe() {
    return rememberMe;
  }

  /**
   * Set the remember me text.
   *
   * @param rememberMe the remember me text
   */
  public void setRememberMe(String rememberMe) {
    this.rememberMe = rememberMe;
  }

  /**
   * Get the login button text.
   *
   * @return the login button text
   */
  public String getSubmit() {
    return submit;
  }

  /**
   * Set the login button text.
   *
   * @param loginButton the login button text
   */
  public void setSubmit(String loginButton) {
    this.submit = loginButton;
  }

  /**
   * Get the cancel button text.
   *
   * @return the cancel button text
   */
  public String getCancel() {
    return cancel;
  }

  /**
   * Set the cancel button text.
   *
   * @param cancelButton the cancel button text
   */
  public void setCancel(String cancelButton) {
    this.cancel = cancelButton;
  }
}
