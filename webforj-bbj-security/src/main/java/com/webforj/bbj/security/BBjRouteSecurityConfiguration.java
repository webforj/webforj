package com.webforj.bbj.security;

import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.Objects;
import java.util.Optional;

/**
 * {@link RouteSecurityConfiguration} of a BBj application, backed by
 * {@link BBjSecurityConfigurationProperties}.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjRouteSecurityConfiguration implements RouteSecurityConfiguration {
  private final BBjSecurityConfigurationProperties properties;

  /**
   * Creates a configuration from the given properties.
   *
   * @param properties the security configuration properties
   *
   * @throws NullPointerException if {@code properties} is null
   */
  BBjRouteSecurityConfiguration(BBjSecurityConfigurationProperties properties) {
    this.properties = Objects.requireNonNull(properties, "properties");
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * When no authentication path is configured, the default authentication location applies.
   * </p>
   */
  @Override
  public Optional<Location> getAuthenticationLocation() {
    return Optional.ofNullable(properties.getAuthenticationPath()).map(Location::new)
        .or(RouteSecurityConfiguration.super::getAuthenticationLocation);
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * When no deny path is configured, the default deny location applies.
   * </p>
   */
  @Override
  public Optional<Location> getDenyLocation() {
    return Optional.ofNullable(properties.getDenyPath()).map(Location::new)
        .or(RouteSecurityConfiguration.super::getDenyLocation);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return Boolean.TRUE.equals(properties.getEnabled());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSecureByDefault() {
    return Boolean.TRUE.equals(properties.getSecureByDefault());
  }

  /**
   * Gets the location the app opens after a sign out.
   *
   * <p>
   * When no logout success path is configured, the authentication location applies.
   * </p>
   *
   * @return the logout location, or empty if neither a logout success path nor an authentication
   *         location is available
   */
  public Optional<Location> getLogoutLocation() {
    return Optional.ofNullable(properties.getLogoutSuccessPath()).map(Location::new)
        .or(this::getAuthenticationLocation);
  }

}
