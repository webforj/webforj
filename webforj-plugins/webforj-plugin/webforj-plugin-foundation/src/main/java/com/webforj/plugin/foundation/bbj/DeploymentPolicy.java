package com.webforj.plugin.foundation.bbj;

import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.util.function.Consumer;

/**
 * Decides the administration credentials of a BBj goal or command from its effective settings.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentPolicy {

  /**
   * The administration credentials a goal accepts.
   */
  public enum Authentication {

    /**
     * A username and password.
     */
    PASSWORD,

    /**
     * An authorization token, or a username and password when the token is blank.
     */
    PASSWORD_OR_TOKEN
  }

  private DeploymentPolicy() {}

  /**
   * Resolves the administration credentials of a goal or command.
   *
   * <p>
   * Without a username and without a password, the credentials are the factory administrator
   * account of BBj. While the factory account is in use,
   * {@link BBjCredentials#FACTORY_ACCOUNT_WARNING} is reported as a warning.
   * </p>
   *
   * @param username the username, null when none is configured
   * @param password the password, null when none is configured
   * @param token the authorization token, null when none is configured
   * @param authentication the credentials the goal accepts
   * @param warn where the warning of the factory account is reported
   *
   * @return the credentials
   * @throws IllegalArgumentException if only one of username and password is given or one is blank,
   *         with a message that names the missing user name, password or both
   */
  public static BBjCredentials resolveCredentials(String username, String password, String token,
      Authentication authentication, Consumer<String> warn) {
    if (authentication == Authentication.PASSWORD_OR_TOKEN && token != null && !token.isBlank()) {
      return BBjCredentials.createToken(token);
    }

    BBjCredentials credentials =
        username == null && password == null ? BBjCredentials.createFactoryAccount()
            : createCredentials(username, password);
    if (credentials.isFactoryAccount()) {
      warn.accept(BBjCredentials.FACTORY_ACCOUNT_WARNING);
    }

    return credentials;
  }

  private static BBjCredentials createCredentials(String username, String password) {
    boolean usernameMissing = username == null || username.isBlank();
    boolean passwordMissing = password == null || password.isBlank();
    if (usernameMissing && passwordMissing) {
      throw new IllegalArgumentException(
          "The BBj administration user name and password are missing");
    }

    if (usernameMissing) {
      throw new IllegalArgumentException("The BBj administration user name is missing");
    }

    if (passwordMissing) {
      throw new IllegalArgumentException("The BBj administration password is missing");
    }

    return BBjCredentials.createUser(username, password);
  }
}
