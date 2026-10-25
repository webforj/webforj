package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

import com.basis.api.simple.ProxyOutputStreamToSimpleInstance;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigValueType;

/**
 * Reads the route security configuration of a BBj application.
 *
 * <p>
 * The settings are the keys {@code enabled}, {@code secure-by-default},
 * {@code authentication-path}
 * and {@code deny-path} below {@code webforj.security}. A key that is not set
 * keeps the default of
 * {@link BBjSecurityConfigurationProperties}. A value of {@code enabled} or
 * {@code secure-by-default} that is not a boolean counts as true. A path that
 * is not a string keeps
 * its default. Each such value is logged as a warning that names the key.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjConfigReader {
  private static final Logger logger = System.getLogger(BBjConfigReader.class.getName());
  private static final String PREFIX = "webforj.security.";
  // Security Keys
  private static final String ENABLED = PREFIX + "enabled";
  private static final String SECURE_BY_DEFAULT = PREFIX + "secure-by-default";
  private static final String AUTHENTICATION_PATH = PREFIX + "authentication-path";
  private static final String DENY_PATH = PREFIX + "deny-path";
  // BBj Keys
  private static final String BBJ_PREFIX = PREFIX + "bbj.";
  private static final String ADMIN_TIMEOUT_SECONDS = BBJ_PREFIX + "admin.timeout-seconds";
  private static final String TOKEN_MINUTES = BBJ_PREFIX + "token.minutes";
  private static final String DEFAULT_SUCCESS_PATH = BBJ_PREFIX + "default-success-path";
  private static final String RETURN_AFTER_LOGIN = BBJ_PREFIX + "return-after-login";
  private static final String LOGOUT_SUCCESS_PATH = BBJ_PREFIX + "logout-success-path";
  // Admin Keys
  private static final String ADMIN_PREFIX = BBJ_PREFIX + "admin.";
  private static final String ADMIN_HOST = ADMIN_PREFIX + "host";
  private static final String ADMIN_PORT = ADMIN_PREFIX + "port";

  private BBjConfigReader() {
  }

  /**
   * Fills the given properties from the application configuration.
   *
   * @param properties the properties to fill
   * @param config     the application configuration
   */
  static void readConfig(BBjSecurityConfigurationProperties properties, Config config) {
    readSecurityKeys(properties, config);
    readAdminKeys(properties, config);
  }

  private static void readSecurityKeys(BBjSecurityConfigurationProperties properties,
      Config config) {
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
  }

  private static void readAdminKeys(BBjSecurityConfigurationProperties properties, Config config) {
    if (config.hasPath(ADMIN_HOST)) {
      readString(config, ADMIN_HOST).ifPresent(properties::setAdminHost);
    }

    if (config.hasPath(ADMIN_PORT)) {
      readInt(config, ADMIN_PORT, 1, 65535).ifPresent(properties::setAdminPort);
    }
    if (config.hasPath(ADMIN_TIMEOUT_SECONDS)) {
      readInt(config, ADMIN_TIMEOUT_SECONDS, 1, 9999).ifPresent(properties::setAdminTimeoutSeconds);
    }
    if (config.hasPath(TOKEN_MINUTES)) {
      readInt(config, TOKEN_MINUTES, 1, 480).ifPresent(properties::setTokenMinutes);
    }
    if (config.hasPath(DEFAULT_SUCCESS_PATH)) {
      readString(config, DEFAULT_SUCCESS_PATH).ifPresent(properties::setDefaultSuccessPath);
    }
    if (config.hasPath(RETURN_AFTER_LOGIN)) {
      properties.setReturnAfterLogin(readBoolean(config, RETURN_AFTER_LOGIN));
    }
    if (config.hasPath(LOGOUT_SUCCESS_PATH)) {
      readString(config, LOGOUT_SUCCESS_PATH).ifPresent(properties::setLogoutSuccessPath);
    }
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
    if (config.getValue(key).valueType() == ConfigValueType.STRING) {
      return Optional.of(config.getString(key));
    }

    logger.log(Level.WARNING, "The configuration key {0} is not a string and keeps its default",
        key);
    return Optional.empty();
  }

  private static Optional<Integer> readInt(Config config, String key, int min, int max) {
    Optional<Integer> value = toInt(config, key);
    if (value.isEmpty()) {
      logger.log(Level.WARNING, "The configuration key {0} is not an integer and keeps its default",
          key);

      return value;
    }

    if (value.get() < min || value.get() > max) {
      logger.log(Level.WARNING,
          "The configuration key {0} is not between {1} and {2} and keeps its default", key,
          String.valueOf(min), String.valueOf(max));

      return Optional.empty();
    }

    return value;
  }

  private static Optional<Integer> toInt(Config config, String key) {
    try {
      Number number = config.getNumber(key);

      return number.doubleValue() == number.intValue() ? Optional.of(number.intValue())
          : Optional.empty();
    } catch (ConfigException.WrongType e) {
      return Optional.empty();
    }
  }

}
