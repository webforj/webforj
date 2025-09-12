package com.webforj.spring.security;

import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Spring Security configuration implementation of {@link RouteSecurityConfiguration}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class SpringRouteSecurityConfiguration implements RouteSecurityConfiguration {

  @Autowired
  private SpringSecurityConfigurationProperties properties;

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
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getAuthenticationLocation() {
    String path = properties.getAuthenticationPath();
    return path != null ? Optional.of(new Location(path)) : Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getInsufficientPermissionsLocation() {
    String path = properties.getInsufficientPermissionsPath();
    return path != null ? Optional.of(new Location(path)) : Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getCustomDenialLocation() {
    String path = properties.getCustomDenialPath();
    return path != null ? Optional.of(new Location(path)) : Optional.empty();
  }
}
