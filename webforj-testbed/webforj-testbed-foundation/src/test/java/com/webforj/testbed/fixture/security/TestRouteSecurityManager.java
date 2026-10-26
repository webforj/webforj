package com.webforj.testbed.fixture.security;

import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteSecurityConfiguration;
import com.webforj.router.security.RouteSecurityContext;

/** Provides the route security manager for tests. */
public class TestRouteSecurityManager extends AbstractRouteSecurityManager {
  private final RouteSecurityConfiguration configuration;

  public TestRouteSecurityManager(RouteSecurityConfiguration configuration) {
    this.configuration = configuration;
  }

  @Override
  public RouteSecurityConfiguration getConfiguration() {
    return configuration;
  }

  @Override
  public RouteSecurityContext getSecurityContext() {
    return new TestRouteSecurityContext();
  }
}
