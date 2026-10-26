package com.webforj.testbed.fixture.security;

import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.Optional;

/** Configures login and denial redirects for security tests. */
public class TestRouteSecurityConfiguration implements RouteSecurityConfiguration {

  @Override
  public Optional<Location> getAuthenticationLocation() {
    return Optional.of(new Location("login"));
  }

  @Override
  public Optional<Location> getDenyLocation() {
    return Optional.of(new Location("denied"));
  }
}
