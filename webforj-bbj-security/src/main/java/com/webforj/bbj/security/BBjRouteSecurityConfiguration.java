package com.webforj.bbj.security;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

/**
 * The route security settings of an app running in BBjServices, read from the
 * {@code webforj.security} configuration keys.
 *
 * <p>
 * Supported keys are {@code enabled}, {@code secure-by-default}, {@code authentication-path} and
 * {@code deny-path}.
 * </p>
 *
 * <p>
 * A key that is not set, or is set to {@code null}, uses its default and is logged at debug level.
 * The defaults are off for {@code enabled}, on for {@code secure-by-default}, {@code /login} for
 * {@code authentication-path} and the authentication location with an {@code error} query parameter
 * for {@code deny-path}.
 * </p>
 *
 * <p>
 * An invalid value is logged once as a warning. An invalid {@code enabled} or
 * {@code secure-by-default} turns the setting on. An invalid path uses the default of its key.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityConfiguration implements RouteSecurityConfiguration {
  private static final String ENABLED = "webforj.security.enabled";
  private static final String SECURE_BY_DEFAULT = "webforj.security.secure-by-default";
  private static final String AUTHENTICATION_PATH = "webforj.security.authentication-path";
  private static final String DENY_PATH = "webforj.security.deny-path";
  private static final String DEFAULT_AUTHENTICATION_PATH = "/login";
  private static final String DEFAULT_DENY_PATH_TEXT =
      "the authentication location with an error query parameter";
  private static final String MISSING_MESSAGE = "{0} is not set, using {1}";
  private static final Logger logger =
      System.getLogger(BBjRouteSecurityConfiguration.class.getName());

  private final boolean enabled;
  private final boolean secureByDefault;
  private final String authenticationPath;
  private final String denyPath;

  private BBjRouteSecurityConfiguration(Builder builder) {
    this.enabled = builder.enabled;
    this.secureByDefault = builder.secureByDefault;
    this.authenticationPath = builder.authenticationPath;
    this.denyPath = builder.denyPath;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSecureByDefault() {
    return secureByDefault;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getAuthenticationLocation() {
    return Optional.of(new Location(authenticationPath));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getDenyLocation() {
    return Optional.ofNullable(denyPath).map(Location::new)
        .or(RouteSecurityConfiguration.super::getDenyLocation);
  }

  /**
   * Reads the configuration from the given config.
   *
   * <p>
   * A key that is not set never throws and uses its default, which is logged at debug level. The
   * defaults are off for {@code webforj.security.enabled}, on for
   * {@code webforj.security.secure-by-default}, {@code /login} for
   * {@code webforj.security.authentication-path} and the authentication location with an
   * {@code error} query parameter for {@code webforj.security.deny-path}.
   * </p>
   *
   * <p>
   * A value that is not a boolean enables security and keeps secure by default on. A path that is
   * blank or not relative to the app uses the default of its key. Every invalid value is logged
   * once as a warning.
   * </p>
   *
   * @param config the config to read
   * @return the configuration
   */
  static BBjRouteSecurityConfiguration create(Config config) {
    final Builder builder = new Builder();
    builder.enabled = readBoolean(config, ENABLED, false, true);
    builder.secureByDefault = readBoolean(config, SECURE_BY_DEFAULT, true, true);
    builder.authenticationPath = readPath(config, AUTHENTICATION_PATH, DEFAULT_AUTHENTICATION_PATH)
        .orElse(DEFAULT_AUTHENTICATION_PATH);
    builder.denyPath = readPath(config, DENY_PATH, DEFAULT_DENY_PATH_TEXT).orElse(null);

    return new BBjRouteSecurityConfiguration(builder);
  }

  private static boolean readBoolean(Config config, String key, boolean missingValue,
      boolean invalidValue) {
    try {
      if (!config.hasPath(key)) {
        logger.log(Level.DEBUG, MISSING_MESSAGE, key, missingValue);
        return missingValue;
      }

      return config.getBoolean(key);
    } catch (ConfigException e) {
      logger.log(Level.WARNING, "Invalid value for {0}, expected true or false, using {1}", key,
          invalidValue);
      return invalidValue;
    }
  }

  private static Optional<String> readPath(Config config, String key, String defaultText) {
    Optional<String> path;
    try {
      if (!config.hasPath(key)) {
        logger.log(Level.DEBUG, MISSING_MESSAGE, key, defaultText);
        return Optional.empty();
      }

      path =
          Optional.of(config.getString(key)).filter(BBjRouteSecurityConfiguration::isRelativePath);
    } catch (ConfigException e) {
      path = Optional.empty();
    }

    if (path.isEmpty()) {
      logger.log(Level.WARNING,
          "Invalid value for {0}, expected a path inside the app starting with a single slash,"
              + " using {1}",
          key, defaultText);
    }

    return path;
  }

  private static boolean isRelativePath(String path) {
    if (!path.startsWith("/") || path.startsWith("//") || path.indexOf('\\') >= 0) {
      return false;
    }

    try {
      new Location(path);
      return true;
    } catch (IllegalArgumentException e) {
      return false;
    }
  }

  private static final class Builder {
    boolean enabled;
    boolean secureByDefault;
    String authenticationPath;
    String denyPath;
  }
}
