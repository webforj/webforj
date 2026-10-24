package com.webforj.bbj.security;

import com.typesafe.config.Config;

/**
 * BBjConfigReader
 */
public final class BBjConfigReader {
  private static final String PREFIX = "webforj.security.";
  private static final String ENABLED = PREFIX + "enabled";
  private static final String SECURE_BY_DEFAULT = PREFIX + "secure-by-default";
  private static final String AUTHENTICATION_PATH = PREFIX + "authentication-path";
  private static final String DENY_PATH = PREFIX + "deny-path";

  private BBjConfigReader() {}

  static BBjRouteSecurityConfiguration readConfig(Config config) {
    var properties = new BBjSecurityConfigurationProperties();
    var enabled = config.getBoolean(ENABLED);
    properties.setEnabled(enabled);
    var secureByDefault = config.getBoolean(SECURE_BY_DEFAULT);
    properties.setSecureByDefault(secureByDefault);
    var authenticationPath = config.getString(AUTHENTICATION_PATH);
    properties.setAuthenticationPath(authenticationPath);
    var denyPath = config.getString(DENY_PATH);
    properties.setDenyPath(denyPath);

    return new BBjRouteSecurityConfiguration(properties);
  }

}
