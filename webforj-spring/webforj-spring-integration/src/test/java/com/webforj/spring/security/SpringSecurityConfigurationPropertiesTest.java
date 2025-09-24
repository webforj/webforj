package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SpringSecurityConfigurationPropertiesTest {

  private SpringSecurityConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new SpringSecurityConfigurationProperties();
  }

  @Nested
  class DefaultValues {

    @Test
    void shouldHaveDefaultEnabledValue() {
      assertTrue(properties.getEnabled());
    }

    @Test
    void shouldHaveDefaultSecureByDefaultValue() {
      assertTrue(properties.getSecureByDefault());
    }

    @Test
    void shouldHaveNullAuthenticationPath() {
      assertNull(properties.getAuthenticationPath());
    }

    @Test
    void shouldHaveNullDenyPath() {
      assertNull(properties.getDenyPath());
    }
  }

  @Nested
  class PropertySetters {

    @Test
    void shouldSetEnabled() {
      properties.setEnabled(false);
      assertFalse(properties.getEnabled());
    }

    @Test
    void shouldSetSecureByDefault() {
      properties.setSecureByDefault(false);
      assertFalse(properties.getSecureByDefault());
    }

    @Test
    void shouldSetAuthenticationPath() {
      properties.setAuthenticationPath("/signin");
      assertEquals("/signin", properties.getAuthenticationPath());
    }

    @Test
    void shouldSetDenyPath() {
      properties.setDenyPath("/access-denied");
      assertEquals("/access-denied", properties.getDenyPath());
    }
  }
}
