package com.webforj.bbj.security;

import java.util.Objects;
import java.util.Optional;

import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;

/**
 * BBjRouteSecurityConfiguration
 */
public class BBjRouteSecurityConfiguration implements RouteSecurityConfiguration {
  private final BBjSecurityConfigurationProperties properties;

  public BBjRouteSecurityConfiguration(BBjSecurityConfigurationProperties properties) {
    this.properties = Objects.requireNonNull(properties, "properties");
  }

  @Override
  public Optional<Location> getAuthenticationLocation() {
    return Optional.ofNullable(properties.getAuthenticationPath()).map(Location::new);
  }

  @Override
  public Optional<Location> getDenyLocation() {
    return Optional.ofNullable(properties.getDenyPath()).map(Location::new);
  }

  @Override
  public boolean isEnabled() {
    return Boolean.TRUE.equals(properties.getEnabled());
  }

  @Override
  public boolean isSecureByDefault() {
    return Boolean.TRUE.equals(properties.getSecureByDefault());
  }

}
