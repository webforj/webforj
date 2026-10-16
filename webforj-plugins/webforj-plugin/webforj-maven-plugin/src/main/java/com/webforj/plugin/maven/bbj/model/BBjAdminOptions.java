package com.webforj.plugin.maven.bbj.model;

import java.util.Objects;

/**
 * Configures BBj administration credentials, a username and password or an authorization token.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjAdminOptions {

  private String username;
  private String password;
  private String token = "";

  /**
   * Gets the administration username.
   *
   * @return the administration username, null when none is configured
   */
  public String getUsername() {
    return username;
  }

  /**
   * Sets the administration username.
   *
   * @param username the administration username
   *
   * @throws NullPointerException if username is null
   */
  public void setUsername(String username) {
    this.username = Objects.requireNonNull(username);
  }

  /**
   * Gets the administration password.
   *
   * @return the administration password, null when none is configured
   */
  public String getPassword() {
    return password;
  }

  /**
   * Sets the administration password.
   *
   * @param password the administration password
   *
   * @throws NullPointerException if password is null
   */
  public void setPassword(String password) {
    this.password = Objects.requireNonNull(password);
  }

  /**
   * Gets the authorization token.
   *
   * @return the authorization token
   */
  public String getToken() {
    return token;
  }

  /**
   * Sets the authorization token, which takes precedence over the username and password when it is
   * not blank.
   *
   * @param token the authorization token
   *
   * @throws NullPointerException if token is null
   */
  public void setToken(String token) {
    this.token = Objects.requireNonNull(token);
  }
}
