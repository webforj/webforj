package com.webforj.testbed.fixture.security;

import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.Optional;

/** Disables login and denial redirects for security tests. */
public class TestRouteSecurityConfigurationNoRedirect implements RouteSecurityConfiguration {

  @Override
  public Optional<Location> getAuthenticationLocation() {
    return Optional.empty();
  }

  @Override
  public Optional<Location> getDenyLocation() {
    return Optional.empty();
  }
}
