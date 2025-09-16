package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.webforj.router.history.Location;
import java.util.Optional;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SpringRouteSecurityConfigurationTest {

  @InjectMocks
  private SpringRouteSecurityConfiguration configuration;

  @Mock
  private SpringSecurityConfigurationProperties properties;

  @Nested
  class EnabledStatus {

    @Test
    void shouldReturnTrueWhenEnabled() {
      when(properties.getEnabled()).thenReturn(Boolean.TRUE);
      assertTrue(configuration.isEnabled());
    }

    @Test
    void shouldReturnFalseWhenDisabled() {
      when(properties.getEnabled()).thenReturn(Boolean.FALSE);
      assertFalse(configuration.isEnabled());
    }

    @Test
    void shouldReturnFalseWhenEnabledIsNull() {
      when(properties.getEnabled()).thenReturn(null);
      assertFalse(configuration.isEnabled());
    }
  }

  @Nested
  class SecureByDefaultStatus {

    @Test
    void shouldReturnTrueWhenSecureByDefaultEnabled() {
      when(properties.getSecureByDefault()).thenReturn(Boolean.TRUE);
      assertTrue(configuration.isSecureByDefault());
    }

    @Test
    void shouldReturnFalseWhenSecureByDefaultDisabled() {
      when(properties.getSecureByDefault()).thenReturn(Boolean.FALSE);
      assertFalse(configuration.isSecureByDefault());
    }

    @Test
    void shouldReturnFalseWhenSecureByDefaultIsNull() {
      when(properties.getSecureByDefault()).thenReturn(null);
      assertFalse(configuration.isSecureByDefault());
    }
  }

  @Nested
  class AuthenticationLocation {

    @Test
    void shouldReturnConfiguredAuthenticationPath() {
      when(properties.getAuthenticationPath()).thenReturn("/signin");

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals("/signin", location.get().toString());
    }

    @Test
    void shouldReturnDefaultLoginWhenAuthenticationPathIsNull() {
      when(properties.getAuthenticationPath()).thenReturn(null);

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals("/login", location.get().toString());
    }

    @Test
    void shouldPreserveQueryParametersInAuthenticationPath() {
      when(properties.getAuthenticationPath()).thenReturn("/auth?redirect=true");

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals("/auth?redirect=true", location.get().toString());
    }

    @Test
    void shouldPreserveFragmentInAuthenticationPath() {
      when(properties.getAuthenticationPath()).thenReturn("/auth#login-form");

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals("/auth#login-form", location.get().toString());
    }
  }

  @Nested
  class DenyLocation {

    @Test
    void shouldReturnConfiguredDenyPath() {
      when(properties.getDenyPath()).thenReturn("/access-denied");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals("/access-denied", location.get().toString());
    }

    @Test
    void shouldReturnSuperDefaultWhenDenyPathIsNull() {
      when(properties.getDenyPath()).thenReturn(null);
      when(properties.getAuthenticationPath()).thenReturn("/login");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertTrue(location.get().toString().contains("/login"));
    }

    @Test
    void shouldHandleEmptyDenyPath() {
      when(properties.getDenyPath()).thenReturn("");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals("/", location.get().toString());
    }

    @Test
    void shouldPreserveComplexDenyPath() {
      when(properties.getDenyPath()).thenReturn("/error/403?type=authorization");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals("/error/403?type=authorization", location.get().toString());
    }
  }
}
