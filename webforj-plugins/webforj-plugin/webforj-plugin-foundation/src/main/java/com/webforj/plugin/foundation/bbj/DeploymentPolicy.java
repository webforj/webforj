package com.webforj.plugin.foundation.bbj;

import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.net.URI;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Decides the administration credentials, the web server port and the archive of a BBj goal from
 * its effective settings.
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

  private static final String USERNAME_KEY = "bbj.admin.username";
  private static final String PASSWORD_KEY = "bbj.admin.password";
  private static final String REQUIREMENT =
      "BBj administration requires " + USERNAME_KEY + " and " + PASSWORD_KEY;
  private static final String TOKEN_REQUIREMENT = REQUIREMENT + ", or webforj.bbj.admin.token";

  private DeploymentPolicy() {}

  /**
   * Resolves the administration credentials of a goal.
   *
   * <p>
   * Without a username and without a password, the credentials are the factory administrator
   * account of BBj.
   * </p>
   *
   * @param username the username, null when none is configured
   * @param password the password, null when none is configured
   * @param token the authorization token, null when none is configured
   * @param authentication the credentials the goal accepts
   *
   * @return the credentials
   * @throws IllegalArgumentException if only one of username and password is given or one is blank
   */
  public static BBjCredentials resolveCredentials(String username, String password, String token,
      Authentication authentication) {
    if (authentication == Authentication.PASSWORD_OR_TOKEN && token != null && !token.isBlank()) {
      return BBjCredentials.ofToken(token);
    }

    String requirement =
        authentication == Authentication.PASSWORD_OR_TOKEN ? TOKEN_REQUIREMENT : REQUIREMENT;
    if (username == null && password == null) {
      return BBjCredentials.ofFactoryAccount();
    }

    if (username == null || username.isBlank()) {
      throw new IllegalArgumentException(formatRequirement(requirement, USERNAME_KEY, username));
    }

    if (password == null || password.isBlank()) {
      throw new IllegalArgumentException(formatRequirement(requirement, PASSWORD_KEY, password));
    }

    return BBjCredentials.of(username, password);
  }

  /**
   * Gets the web server port of a BBjShip base address.
   *
   * @param url the base address
   *
   * @return the port of the address, or the default port of its scheme
   */
  public static int getPort(URI url) {
    if (url.getPort() != -1) {
      return url.getPort();
    }

    return "https".equalsIgnoreCase(url.getScheme()) ? 443 : 80;
  }

  /**
   * Resolves the deployment archive of a publish name, {@code bbj/<publishName>.zip} in the build
   * directory.
   *
   * @param buildDirectory the build directory of the project
   * @param publishName the publish name the archive is named after
   *
   * @return the archive
   * @throws IllegalArgumentException if the publish name is blank or invalid
   */
  public static Path resolveArchive(Path buildDirectory, String publishName) {
    if (publishName.isBlank()) {
      throw new IllegalArgumentException(
          "BBj deployment requires bbj.publishName or a final name of the build");
    }

    Optional<String> refusal = PublishNamePolicy.findRefusal(publishName);
    if (refusal.isPresent()) {
      throw new IllegalArgumentException(refusal.get());
    }

    return buildDirectory.resolve("bbj").resolve(publishName + ".zip");
  }

  private static String formatRequirement(String requirement, String key, String value) {
    return requirement + ". " + key + (value == null ? " is missing" : " is empty");
  }
}
