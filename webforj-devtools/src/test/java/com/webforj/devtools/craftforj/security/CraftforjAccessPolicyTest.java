package com.webforj.devtools.craftforj.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CraftforjAccessPolicyTest {

  private static Config config(Map<String, Object> values) {
    return ConfigFactory.parseMap(values);
  }

  @Nested
  @DisplayName("enabled")
  class Enabled {

    @Test
    @DisplayName("Should be off without configuration, since craftforJ is opt in")
    void shouldBeOffWithoutConfiguration() {
      assertFalse(CraftforjAccessPolicy.isEnabled(null));
      assertFalse(CraftforjAccessPolicy.isEnabled(ConfigFactory.empty()));
      assertFalse(CraftforjAccessPolicy.isAllowed(ConfigFactory.empty(), "127.0.0.1"));
    }

    @Test
    @DisplayName("Should be on when the configuration turns it on")
    void shouldBeOnWhenConfigured() {
      Config config = config(Map.of(CraftforjAccessPolicy.KEY_ENABLED, true));

      assertTrue(CraftforjAccessPolicy.isEnabled(config));
      assertTrue(CraftforjAccessPolicy.isAllowed(config, "127.0.0.1"));
    }

    @Test
    @DisplayName("Should be off when the configuration turns it off")
    void shouldBeOffWhenConfigured() {
      Config config = config(Map.of(CraftforjAccessPolicy.KEY_ENABLED, false));

      assertFalse(CraftforjAccessPolicy.isEnabled(config));
      assertFalse(CraftforjAccessPolicy.isAllowed(config, "127.0.0.1"));
    }
  }

  @Nested
  @DisplayName("hosts")
  class Hosts {

    @ParameterizedTest
    @ValueSource(strings = {"127.0.0.1", "127.1.2.3", "::1", "0:0:0:0:0:0:0:1", "[::1]",
        "::ffff:127.0.0.1", "localhost", "::1%lo0"})
    @DisplayName("Should allow loopback in every spelling")
    void shouldAllowLoopback(String address) {
      assertTrue(CraftforjAccessPolicy.isAllowedHost(null, address));
    }

    @ParameterizedTest
    @ValueSource(strings = {"203.0.113.7", "10.0.0.5", "2001:db8::1", "::2", "::", "fe80::1",
        "127notanaddress"})
    @DisplayName("Should refuse anything else without an allow list")
    void shouldRefuseRemote(String address) {
      assertFalse(CraftforjAccessPolicy.isAllowedHost(null, address));
    }

    @Test
    @DisplayName("Should refuse a client whose address cannot be read")
    void shouldRefuseUnknownAddress() {
      assertFalse(CraftforjAccessPolicy.isAllowedHost(null, null));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(null, "  "));
    }

    @Test
    @DisplayName("Should allow an address named in the allow list")
    void shouldAllowListedAddress() {
      Config config = config(
          Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, List.of("203.0.113.7", "198.51.100.4")));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "203.0.113.7"));
      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "198.51.100.4"));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(config, "198.51.100.5"));
    }

    @Test
    @DisplayName("Should allow a prefix entry")
    void shouldAllowPrefixEntry() {
      Config config =
          config(Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, List.of("192.168.1.*")));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "192.168.1.42"));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(config, "192.168.2.42"));
    }

    @Test
    @DisplayName("Should drop the check for a single wildcard entry")
    void shouldDropTheCheckForWildcard() {
      Config config = config(Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, List.of("*")));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "203.0.113.7"));
      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, null));
    }

    @Test
    @DisplayName("Should ignore blank entries in the allow list")
    void shouldIgnoreBlankEntries() {
      Config config =
          config(Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, List.of("  ", "203.0.113.7")));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "127.0.0.1"));
      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "203.0.113.7"));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(config, "198.51.100.4"));
    }

    @Test
    @DisplayName("Should read a plain string as a one entry allow list")
    void shouldReadPlainStringAsOneEntry() {
      Config config = config(Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, "203.0.113.7"));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "203.0.113.7"));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(config, "198.51.100.4"));
    }

    @Test
    @DisplayName("Should refuse everything remote when the allow list is empty")
    void shouldRefuseWhenAllowListEmpty() {
      Config config = config(Map.of(CraftforjAccessPolicy.KEY_HOSTS_ALLOWED, List.of()));

      assertTrue(CraftforjAccessPolicy.isAllowedHost(config, "127.0.0.1"));
      assertFalse(CraftforjAccessPolicy.isAllowedHost(config, "203.0.113.7"));
    }
  }
}
