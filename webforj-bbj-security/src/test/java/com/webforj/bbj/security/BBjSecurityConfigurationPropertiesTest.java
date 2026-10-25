package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BBjSecurityConfigurationPropertiesTest {

  private BBjSecurityConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new BBjSecurityConfigurationProperties();
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

    @Test
    void shouldHaveNullLogoutSuccessPath() {
      assertNull(properties.getLogoutSuccessPath());
    }

    @Test
    void shouldHaveDefaultAdminHost() {
      assertEquals("127.0.0.1", properties.getAdminHost());
    }

    @Test
    void shouldHaveDefaultAdminPort() {
      assertEquals(2002, properties.getAdminPort());
    }

    @Test
    void shouldHaveDefaultAdminTimeoutSeconds() {
      assertEquals(10, properties.getAdminTimeoutSeconds());
    }

    @Test
    void shouldHaveDefaultTokenMinutes() {
      assertEquals(30, properties.getTokenMinutes());
    }

    @Test
    void shouldHaveDefaultSuccessPath() {
      assertEquals("/", properties.getDefaultSuccessPath());
    }

    @Test
    void shouldHaveDefaultReturnAfterLogin() {
      assertTrue(properties.getReturnAfterLogin());
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

    @Test
    void shouldSetLogoutSuccessPath() {
      properties.setLogoutSuccessPath("/goodbye");
      assertEquals("/goodbye", properties.getLogoutSuccessPath());
    }

    @Test
    void shouldSetAdminHost() {
      properties.setAdminHost("bbj.example.com");
      assertEquals("bbj.example.com", properties.getAdminHost());
    }

    @Test
    void shouldSetAdminPort() {
      properties.setAdminPort(2020);
      assertEquals(2020, properties.getAdminPort());
    }

    @Test
    void shouldSetAdminTimeoutSeconds() {
      properties.setAdminTimeoutSeconds(5);
      assertEquals(5, properties.getAdminTimeoutSeconds());
    }

    @Test
    void shouldSetTokenMinutes() {
      properties.setTokenMinutes(15);
      assertEquals(15, properties.getTokenMinutes());
    }

    @Test
    void shouldSetDefaultSuccessPath() {
      properties.setDefaultSuccessPath("/home");
      assertEquals("/home", properties.getDefaultSuccessPath());
    }

    @Test
    void shouldSetReturnAfterLogin() {
      properties.setReturnAfterLogin(false);
      assertFalse(properties.getReturnAfterLogin());
    }
  }
}
