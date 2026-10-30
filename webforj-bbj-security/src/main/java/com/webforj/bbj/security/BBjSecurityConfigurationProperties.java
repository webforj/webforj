package com.webforj.bbj.security;

import com.google.common.net.InetAddresses;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigResolveOptions;
import com.typesafe.config.ConfigValueType;
import java.io.File;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.util.Optional;

/**
 * Route security configuration properties of a BBj application.
 *
 * <p>
 * Security is enabled and routes are secure by default unless set otherwise. No authentication
 * path, no deny path and no logout success path are set by default.
 * </p>
 *
 * <p>
 * Substitutions such as {@code ${?NAME}} in the keys are resolved against the configuration and the
 * environment. This resolution is removed once webforJ resolves the configuration itself.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjSecurityConfigurationProperties {
  private static final Logger logger =
      System.getLogger(BBjSecurityConfigurationProperties.class.getName());
  private static final String LOCALHOST = "localhost";
  private static final String PREFIX = "webforj.security.";
  // Security Keys
  private static final String ENABLED = PREFIX + "enabled";
  private static final String SECURE_BY_DEFAULT = PREFIX + "secure-by-default";
  private static final String AUTHENTICATION_PATH = PREFIX + "authentication-path";
  private static final String DENY_PATH = PREFIX + "deny-path";
  // BBj Keys
  private static final String BBJ_PREFIX = PREFIX + "bbj.";
  private static final String DEFAULT_SUCCESS_PATH = BBJ_PREFIX + "default-success-path";
  private static final String RETURN_AFTER_LOGIN = BBJ_PREFIX + "return-after-login";
  private static final String LOGOUT_SUCCESS_PATH = BBJ_PREFIX + "logout-success-path";
  // Token Keys
  private static final String TOKEN_PREFIX = BBJ_PREFIX + "token.";
  private static final String TOKEN_MINUTES = TOKEN_PREFIX + "minutes";
  private static final String CHECK_SECONDS = TOKEN_PREFIX + "check-seconds";
  private static final String RENEW_BEFORE_MINUTES = TOKEN_PREFIX + "renew-before-minutes";
  // Admin Keys
  private static final String ADMIN_PREFIX = BBJ_PREFIX + "admin.";
  private static final String ADMIN_TIMEOUT_SECONDS = ADMIN_PREFIX + "timeout-seconds";
  private static final String ADMIN_HOST = ADMIN_PREFIX + "host";
  private static final String ADMIN_PORT = ADMIN_PREFIX + "port";
  private static final String ADMIN_SSL = ADMIN_PREFIX + "ssl";
  private static final String ADMIN_TRUSTSTORE = ADMIN_PREFIX + "truststore";
  private static final String ADMIN_TRUSTSTORE_PASSWORD = ADMIN_PREFIX + "truststore-password";
  // Throttle Keys
  private static final String THROTTLE_PREFIX = BBJ_PREFIX + "throttle.";
  private static final String THROTTLE_MAX_ATTEMPTS = THROTTLE_PREFIX + "max-attempts";
  private static final String THROTTLE_ADDRESS_MAX_ATTEMPTS =
      THROTTLE_PREFIX + "address-max-attempts";
  private static final String THROTTLE_WINDOW_SECONDS = THROTTLE_PREFIX + "window-seconds";
  private boolean configResolved;

  private Boolean enabled = true;
  private Boolean secureByDefault = true;
  private String authenticationPath;
  private String denyPath;
  private String adminHost = "127.0.0.1";
  private Integer adminPort = 2002;
  private Integer adminTimeoutSeconds = 10;
  private Integer tokenMinutes = 30;
  private Integer checkSeconds = 30;
  private Integer renewBeforeMinutes = 5;
  private String defaultSuccessPath = "/";
  private Boolean returnAfterLogin = true;
  private String logoutSuccessPath;
  private Integer throttleMaxAttempts = 5;
  private Integer throttleAddressMaxAttempts = 20;
  private Integer throttleWindowSeconds = 300;
  private Boolean adminSsl = false;
  private String adminTruststore;
  private String adminTruststorePassword;

  BBjSecurityConfigurationProperties(Config config) {
    Config resolved;
    try {
      resolved = config.resolve(ConfigResolveOptions.defaults().setAllowUnresolved(true));
      configResolved = true;
    } catch (RuntimeException e) {
      configResolved = false;
      return;
    }
    readSecurityKeys(this, resolved);
    readBBjKeys(this, resolved);
    readTokenKeys(this, resolved);
    readAdminKeys(this, resolved);
    readThrottleKeys(this, resolved);
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

  private static void readBBjKeys(BBjSecurityConfigurationProperties properties, Config config) {
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

  private static void readTokenKeys(BBjSecurityConfigurationProperties properties, Config config) {
    if (config.hasPath(TOKEN_MINUTES)) {
      readInt(config, TOKEN_MINUTES, 1, Integer.MAX_VALUE).ifPresent(properties::setTokenMinutes);
    }
    if (config.hasPath(CHECK_SECONDS)) {
      readInt(config, CHECK_SECONDS, 0, 3600).ifPresent(properties::setCheckSeconds);
    }
    if (config.hasPath(RENEW_BEFORE_MINUTES)) {
      readInt(config, RENEW_BEFORE_MINUTES, 0, properties.getTokenMinutes())
          .ifPresent(properties::setRenewBeforeMinutes);
    }
  }

  private static void readThrottleKeys(BBjSecurityConfigurationProperties properties,
      Config config) {
    if (config.hasPath(THROTTLE_MAX_ATTEMPTS)) {
      readInt(config, THROTTLE_MAX_ATTEMPTS, 1, 1000).ifPresent(properties::setThrottleMaxAttempts);
    }
    if (config.hasPath(THROTTLE_ADDRESS_MAX_ATTEMPTS)) {
      readInt(config, THROTTLE_ADDRESS_MAX_ATTEMPTS, 0, 10000)
          .ifPresent(properties::setThrottleAddressMaxAttempts);
    }
    if (config.hasPath(THROTTLE_WINDOW_SECONDS)) {
      readInt(config, THROTTLE_WINDOW_SECONDS, 10, 86400)
          .ifPresent(properties::setThrottleWindowSeconds);
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
    if (config.hasPath(ADMIN_SSL)) {
      properties.setAdminSsl(readBoolean(config, ADMIN_SSL));
    }
    if (config.hasPath(ADMIN_TRUSTSTORE)) {
      readString(config, ADMIN_TRUSTSTORE).ifPresent(properties::setAdminTruststore);
    }
    if (config.hasPath(ADMIN_TRUSTSTORE_PASSWORD)) {
      readString(config, ADMIN_TRUSTSTORE_PASSWORD)
          .ifPresent(properties::setAdminTruststorePassword);
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

  /**
   * Gets whether security is enabled.
   *
   * @return true if security is enabled, false otherwise
   */
  public Boolean getEnabled() {
    return enabled;
  }

  /**
   * Sets whether security is enabled.
   *
   * @param enabled true to enable security, false to disable
   */
  public void setEnabled(Boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Gets whether routes are secure by default.
   *
   * @return true if routes are secure by default, false otherwise
   */
  public Boolean getSecureByDefault() {
    return secureByDefault;
  }

  /**
   * Sets whether routes are secure by default.
   *
   * @param secureByDefault true to make routes secure by default, false otherwise
   */
  public void setSecureByDefault(Boolean secureByDefault) {
    this.secureByDefault = secureByDefault;
  }

  /**
   * Gets the authentication path.
   *
   * @return the authentication path, or null if none is set
   */
  public String getAuthenticationPath() {
    return authenticationPath;
  }

  /**
   * Sets the authentication path.
   *
   * @param authenticationPath the authentication path
   */
  public void setAuthenticationPath(String authenticationPath) {
    this.authenticationPath = authenticationPath;
  }

  /**
   * Gets the deny path.
   *
   * @return the deny path, or null if none is set
   */
  public String getDenyPath() {
    return denyPath;
  }

  /**
   * Sets the deny path.
   *
   * @param denyPath the deny path
   */
  public void setDenyPath(String denyPath) {
    this.denyPath = denyPath;
  }

  /**
   * Gets the host of the BBj Admin API.
   *
   * @return the host
   */
  public String getAdminHost() {
    return adminHost;
  }

  /**
   * Sets the host of the BBj Admin API.
   *
   * @param adminHost the host
   */
  public void setAdminHost(String adminHost) {
    this.adminHost = adminHost;
  }

  /**
   * Gets the port of the BBj Admin API.
   *
   * @return the port
   */
  public Integer getAdminPort() {
    return adminPort;
  }

  /**
   * Sets the port of the BBj Admin API.
   *
   * @param adminPort the port
   */
  public void setAdminPort(Integer adminPort) {
    this.adminPort = adminPort;
  }

  /**
   * Gets the time limit of an Admin API call in seconds.
   *
   * @return the time limit in seconds
   */
  public Integer getAdminTimeoutSeconds() {
    return adminTimeoutSeconds;
  }

  /**
   * Sets the time limit of an Admin API call in seconds.
   *
   * @param adminTimeoutSeconds the time limit in seconds
   */
  public void setAdminTimeoutSeconds(Integer adminTimeoutSeconds) {
    this.adminTimeoutSeconds = adminTimeoutSeconds;
  }

  /**
   * Gets the lifetime of a login token in minutes.
   *
   * @return the lifetime in minutes
   */
  public Integer getTokenMinutes() {
    return tokenMinutes;
  }

  /**
   * Sets the lifetime of a login token in minutes.
   *
   * @param tokenMinutes the lifetime in minutes
   */
  public void setTokenMinutes(Integer tokenMinutes) {
    this.tokenMinutes = tokenMinutes;
  }

  /**
   * Gets the seconds between two checks of a login token against the server.
   *
   * @return the seconds between two checks, 0 for a check at every navigation
   */
  public Integer getCheckSeconds() {
    return checkSeconds;
  }

  /**
   * Sets the seconds between two checks of a login token against the server.
   *
   * @param checkSeconds the seconds between two checks
   */
  public void setCheckSeconds(Integer checkSeconds) {
    this.checkSeconds = checkSeconds;
  }

  /**
   * Gets the minutes before the expiry of a login token in which a check renews the token.
   *
   * @return the minutes before the expiry, 0 for no renewal
   */
  public Integer getRenewBeforeMinutes() {
    return renewBeforeMinutes;
  }

  /**
   * Sets the minutes before the expiry of a login token in which a check renews the token.
   *
   * @param renewBeforeMinutes the minutes before the expiry
   */
  public void setRenewBeforeMinutes(Integer renewBeforeMinutes) {
    this.renewBeforeMinutes = renewBeforeMinutes;
  }

  /**
   * Gets the path opened after a sign in when no requested location is used.
   *
   * @return the default success path
   */
  public String getDefaultSuccessPath() {
    return defaultSuccessPath;
  }

  /**
   * Sets the path opened after a sign in when no requested location is used.
   *
   * @param defaultSuccessPath the default success path
   */
  public void setDefaultSuccessPath(String defaultSuccessPath) {
    this.defaultSuccessPath = defaultSuccessPath;
  }

  /**
   * Gets whether the user returns to the requested location after a sign in.
   *
   * @return true if the user returns to the requested location, false otherwise
   */
  public Boolean getReturnAfterLogin() {
    return returnAfterLogin;
  }

  /**
   * Sets whether the user returns to the requested location after a sign in.
   *
   * @param returnAfterLogin true to return to the requested location, false otherwise
   */
  public void setReturnAfterLogin(Boolean returnAfterLogin) {
    this.returnAfterLogin = returnAfterLogin;
  }

  /**
   * Gets the path opened after a sign out.
   *
   * @return the logout success path, or null if none is set
   */
  public String getLogoutSuccessPath() {
    return logoutSuccessPath;
  }

  /**
   * Sets the path opened after a sign out.
   *
   * @param logoutSuccessPath the logout success path
   */
  public void setLogoutSuccessPath(String logoutSuccessPath) {
    this.logoutSuccessPath = logoutSuccessPath;
  }

  /**
   * Gets the failed sign ins of one user name after which the user name is refused.
   *
   * @return the failed sign ins inside one window, 1 to 1000
   */
  public Integer getThrottleMaxAttempts() {
    return throttleMaxAttempts;
  }

  /**
   * Sets the failed sign ins of one user name after which the user name is refused.
   *
   * @param throttleMaxAttempts the failed sign ins inside one window
   */
  public void setThrottleMaxAttempts(Integer throttleMaxAttempts) {
    this.throttleMaxAttempts = throttleMaxAttempts;
  }

  /**
   * Gets the failed sign ins from one client address after which the address is refused.
   *
   * @return the failed sign ins inside one window, 0 to 10000, 0 for no address limit
   */
  public Integer getThrottleAddressMaxAttempts() {
    return throttleAddressMaxAttempts;
  }

  /**
   * Sets the failed sign ins from one client address after which the address is refused.
   *
   * @param throttleAddressMaxAttempts the failed sign ins inside one window, 0 for no address limit
   */
  public void setThrottleAddressMaxAttempts(Integer throttleAddressMaxAttempts) {
    this.throttleAddressMaxAttempts = throttleAddressMaxAttempts;
  }

  /**
   * Gets the seconds a counter of failed sign ins counts from its first failed sign in.
   *
   * @return the seconds of one window, 10 to 86400
   */
  public Integer getThrottleWindowSeconds() {
    return throttleWindowSeconds;
  }

  /**
   * Sets the seconds a counter of failed sign ins counts from its first failed sign in.
   *
   * @param throttleWindowSeconds the seconds of one window
   */
  public void setThrottleWindowSeconds(Integer throttleWindowSeconds) {
    this.throttleWindowSeconds = throttleWindowSeconds;
  }

  /**
   * Gets whether the connection to the BBj Admin API uses SSL.
   *
   * @return true if the connection uses SSL, false otherwise
   */
  public Boolean getAdminSsl() {
    return adminSsl;
  }

  /**
   * Sets whether the connection to the BBj Admin API uses SSL.
   *
   * @param adminSsl true to use SSL, false otherwise
   */
  public void setAdminSsl(Boolean adminSsl) {
    this.adminSsl = adminSsl;
  }

  /**
   * Gets the path of the truststore that checks the certificate of the BBj Admin API.
   *
   * @return the path of the truststore, or null if none is set
   */
  public String getAdminTruststore() {
    return adminTruststore;
  }

  /**
   * Sets the path of the truststore that checks the certificate of the BBj Admin API.
   *
   * @param adminTruststore the path of the truststore
   */
  public void setAdminTruststore(String adminTruststore) {
    this.adminTruststore = adminTruststore;
  }

  /**
   * Gets the password of the truststore.
   *
   * @return the password of the truststore, or null if none is set
   */
  String getAdminTruststorePassword() {
    return adminTruststorePassword;
  }

  /**
   * Sets the password of the truststore.
   *
   * @param adminTruststorePassword the password of the truststore
   */
  public void setAdminTruststorePassword(String adminTruststorePassword) {
    this.adminTruststorePassword = adminTruststorePassword;
  }

  /**
   * Finds the first setting of the Admin API connection that is refused.
   *
   * <p>
   * A host other than {@code localhost} or a loopback address needs SSL and a truststore. A
   * truststore used with SSL must be a file that loads with its password. The text names the keys
   * and never a value.
   * </p>
   *
   * @return the text of the refused setting, or an empty optional when the connection is allowed
   */
  Optional<String> findAdminConnectionProblem() {
    if (!configResolved) {
      return Optional.of("config could not be resolved");
    }

    if (adminTruststore != null && !adminSsl) {
      logger.log(Level.WARNING, "The configuration key {0} is ignored while {1} is false",
          ADMIN_TRUSTSTORE, ADMIN_SSL);
    }

    if (adminTruststore != null && adminSsl) {
      Optional<String> problem = findTruststoreProblem(adminTruststore, adminTruststorePassword);
      if (problem.isPresent()) {
        return problem;
      }
    }

    if (isLoopbackHost(adminHost)) {
      return Optional.empty();
    }

    if (!adminSsl) {
      return Optional
          .of("%s is not a loopback address, so %s must be true".formatted(ADMIN_HOST, ADMIN_SSL));
    }

    if (adminTruststore == null) {
      return Optional.of("%s is not a loopback address, so %s must be set".formatted(ADMIN_HOST,
          ADMIN_TRUSTSTORE));
    }

    return Optional.empty();
  }

  private static boolean isLoopbackHost(String host) {
    if (LOCALHOST.equalsIgnoreCase(host)) {
      return true;
    }

    return InetAddresses.isInetAddress(host) && InetAddresses.forString(host).isLoopbackAddress();
  }

  private static Optional<String> findTruststoreProblem(String path, String password) {
    File file = new File(path);
    if (!file.isFile() || !file.canRead()) {
      return Optional.of("%s must name a readable file".formatted(ADMIN_TRUSTSTORE));
    }

    try {
      var keyStore = KeyStore.getInstance(file, password == null ? null : password.toCharArray());

      if (keyStore.size() == 0) {
        return Optional.of("%s holds no certificates".formatted(ADMIN_TRUSTSTORE));
      }

      return Optional.empty();
    } catch (IOException | GeneralSecurityException e) {
      return Optional
          .of("%s cannot be read with %s".formatted(ADMIN_TRUSTSTORE, ADMIN_TRUSTSTORE_PASSWORD));
    }
  }
}
