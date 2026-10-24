package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;

/**
 * Reads the route security configuration of a BBj application.
 *
 * <p>
 * The settings are the keys {@code enabled}, {@code secure-by-default}, {@code authentication-path}
 * and {@code deny-path} below {@code webforj.security}. A key that is not set keeps the default of
 * {@link BBjSecurityConfigurationProperties}. A value of {@code enabled} or
 * {@code secure-by-default} that is not a boolean counts as true. A path that is not a string keeps
 * its default. Each such value is logged as a warning that names the key.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjConfigReader {
  private static final Logger logger = System.getLogger(BBjConfigReader.class.getName());
  private static final String PREFIX = "webforj.security.";
  private static final String ENABLED = PREFIX + "enabled";
  private static final String SECURE_BY_DEFAULT = PREFIX + "secure-by-default";
  private static final String AUTHENTICATION_PATH = PREFIX + "authentication-path";
  private static final String DENY_PATH = PREFIX + "deny-path";

  private BBjConfigReader() {}

  /**
   * Reads the route security configuration from the application configuration.
   *
   * @param config the application configuration
   *
   * @return the route security configuration
   */
  static BBjRouteSecurityConfiguration readConfig(Config config) {
    var properties = new BBjSecurityConfigurationProperties();

    if (config.hasPath(ENABLED)) {
      properties.setEnabled(readBoolean(config, ENABLED));
    }

    if (config.hasPath(SECURE_BY_DEFAULT)) {
      properties.setSecureByDefault(readBoolean(config, SECURE_BY_DEFAULT));
    }

    if (config.hasPath(AUTHENTICATION_PATH)) {
      readString(config, AUTHENTICATION_PATH).ifPresent(properties::setAuthenticationPath);
    }

    if (config.hasPath(DENY_PATH)) {
      readString(config, DENY_PATH).ifPresent(properties::setDenyPath);
    }

    return new BBjRouteSecurityConfiguration(properties);
  }

  private static boolean readBoolean(Config config, String key) {
    try {
      return config.getBoolean(key);
    } catch (ConfigException.WrongType e) {
      logger.log(Level.WARNING, "The configuration key {0} is not a boolean and counts as true",
          key);
      return true;
    }
  }

  private static Optional<String> readString(Config config, String key) {
    try {
      return Optional.of(config.getString(key));
    } catch (ConfigException.WrongType e) {
      logger.log(Level.WARNING, "The configuration key {0} is not a string and keeps its default",
          key);
      return Optional.empty();
    }
  }

}
