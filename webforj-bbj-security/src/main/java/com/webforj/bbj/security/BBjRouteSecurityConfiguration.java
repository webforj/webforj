package com.webforj.bbj.security;

import com.typesafe.config.Config;
import com.webforj.router.security.RouteSecurityConfiguration;

/**
 * The route security settings of an app running in BBjServices, read from the
 * {@code webforj.security} configuration keys.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityConfiguration implements RouteSecurityConfiguration {
  static final String ENABLED = "webforj.security.enabled";

  private final boolean enabled;

  private BBjRouteSecurityConfiguration(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Reads the configuration from the given config.
   *
   * @param config the config to read
   * @return the configuration, not enabled when the key is not set
   */
  static BBjRouteSecurityConfiguration create(Config config) {
    boolean enabled = config.hasPath(ENABLED) && config.getBoolean(ENABLED);

    return new BBjRouteSecurityConfiguration(enabled);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return enabled;
  }
}
