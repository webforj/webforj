package com.webforj.plugin.foundation.bbj.model;

import java.util.Objects;

/**
 * Administration credentials of a BBj host, either a username and password or an authorization
 * token.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class BBjCredentials {

  /**
   * The warning shown while the factory administrator account of BBj is in use.
   */
  public static final String FACTORY_ACCOUNT_WARNING = "Warning: the BBj factory administrator "
      + "account admin is in use with its factory password. Change the password in Enterprise "
      + "Manager and configure the new credentials";

  private static final String FACTORY_USERNAME = "admin";
  @SuppressWarnings("java:S2068")
  private static final String FACTORY_PASSWORD = "admin123";

  private final String username;
  private final String password;
  private final String token;

  private BBjCredentials(String username, String password, String token) {
    this.username = username;
    this.password = password;
    this.token = token;
  }

  /**
   * Creates credentials of an administration user.
   *
   * @param username the administration username
   * @param password the administration password
   *
   * @return the credentials
   * @throws NullPointerException if an argument is null
   */
  public static BBjCredentials createUser(String username, String password) {
    return new BBjCredentials(Objects.requireNonNull(username), Objects.requireNonNull(password),
        null);
  }

  /**
   * Creates credentials of an authorization token issued by BBjServices.
   *
   * @param token the authorization token
   *
   * @return the credentials
   * @throws NullPointerException if token is null
   */
  public static BBjCredentials createToken(String token) {
    return new BBjCredentials(null, null, Objects.requireNonNull(token));
  }

  /**
   * Creates credentials of the factory administrator account of BBj.
   *
   * @return the credentials
   */
  public static BBjCredentials createFactoryAccount() {
    return createUser(FACTORY_USERNAME, FACTORY_PASSWORD);
  }

  /**
   * Gets the administration username.
   *
   * @return the username, null for token credentials
   */
  public String getUsername() {
    return username;
  }

  /**
   * Gets the administration password.
   *
   * @return the password, null for token credentials
   */
  public String getPassword() {
    return password;
  }

  /**
   * Gets the authorization token.
   *
   * @return the token, null for username and password credentials
   */
  public String getToken() {
    return token;
  }

  /**
   * Checks whether these credentials carry an authorization token.
   *
   * @return true for token credentials
   */
  public boolean isToken() {
    return token != null;
  }

  /**
   * Checks whether these credentials are the factory administrator account of BBj.
   *
   * @return true for the factory username with the factory password
   */
  public boolean isFactoryAccount() {
    return FACTORY_USERNAME.equals(username) && FACTORY_PASSWORD.equals(password);
  }
}
