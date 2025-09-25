package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.router.history.Location;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RouteSecurityConfigurationTest {

  private TestSecurityConfiguration configuration;

  @BeforeEach
  void setUp() {
    configuration = new TestSecurityConfiguration();
  }

  @Test
  void shouldBeEnabledByDefault() {
    assertTrue(configuration.isEnabled());
  }

  @Test
  void shouldBeSecureByDefault() {
    assertTrue(configuration.isSecureByDefault());
  }

  @Test
  void shouldManageAuthenticationLocation() {
    assertFalse(configuration.getAuthenticationLocation().isPresent());

    Location loginLocation = new Location("/login");
    configuration.setAuthenticationLocation(loginLocation);

    assertTrue(configuration.getAuthenticationLocation().isPresent());
    assertEquals(loginLocation, configuration.getAuthenticationLocation().get());
  }

  @Test
  void shouldManageDenyLocation() {
    assertFalse(configuration.getDenyLocation().isPresent());

    Location denyLocation = new Location("/access-denied");
    configuration.setDenyLocation(denyLocation);

    assertTrue(configuration.getDenyLocation().isPresent());
    assertEquals(denyLocation, configuration.getDenyLocation().get());
  }

  private static class TestSecurityConfiguration implements RouteSecurityConfiguration {
    private Location authenticationLocation;
    private Location denyLocation;
    private boolean enabled = true;
    private boolean secureByDefault = true;

    @Override
    public Optional<Location> getAuthenticationLocation() {
      return Optional.ofNullable(authenticationLocation);
    }

    public void setAuthenticationLocation(Location location) {
      this.authenticationLocation = location;
    }

    @Override
    public Optional<Location> getDenyLocation() {
      return Optional.ofNullable(denyLocation);
    }

    public void setDenyLocation(Location location) {
      this.denyLocation = location;
    }

    @Override
    public boolean isEnabled() {
      return enabled;
    }

    @Override
    public boolean isSecureByDefault() {
      return secureByDefault;
    }
  }
}
