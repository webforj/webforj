package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.ConfigFactory;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class McpAppOptionsTest {

  @BeforeEach
  @AfterEach
  void clearProperties() {
    System.clearProperty(McpAppOptions.KEY_ORIGIN);
    System.clearProperty(McpAppOptions.KEY_ALLOWED_ORIGINS);
    System.clearProperty(McpAppOptions.KEY_RESOURCE_DOMAINS);
    System.clearProperty(McpAppOptions.KEY_CONNECT_DOMAINS);
  }

  @Test
  @DisplayName("Should fall back to the defaults without configuration")
  void shouldFallBackToDefaults() {
    McpAppOptions options = McpAppOptions.from(null);

    assertNull(options.getOrigin());
    assertTrue(options.getAllowedOrigins().isEmpty());
    assertTrue(options.getResourceDomains().isEmpty());
    assertTrue(options.getConnectDomains().isEmpty());
  }

  @Test
  @DisplayName("Should read every key from a configuration")
  void shouldReadKeysFromConfiguration() {
    McpAppOptions options = McpAppOptions.from(ConfigFactory.parseString("""
        webforj.origin = "https://demo.example/"
        webforj.mcp.allowed-origins = ["https://host.example"]
        webforj.mcp.resource-domains = ["https://tiles.example"]
        webforj.mcp.connect-domains = ["https://api.example"]
        """));

    assertEquals("https://demo.example/", options.getOrigin());
    assertEquals(List.of("https://host.example"), options.getAllowedOrigins());
    assertEquals(List.of("https://tiles.example"), options.getResourceDomains());
    assertEquals(List.of("https://api.example"), options.getConnectDomains());
  }

  @Test
  @DisplayName("Should read the deployment configuration file")
  void shouldReadDeploymentConfiguration() {
    McpAppOptions options = McpAppOptions.load();

    assertEquals("http://conf.example", options.getOrigin());
    assertEquals(List.of("http://host-a.example", "http://host-b.example"),
        options.getAllowedOrigins());
  }

  @Test
  @DisplayName("Should let a system property override the deployment configuration")
  void shouldLetSystemPropertyOverrideFile() {
    System.setProperty(McpAppOptions.KEY_ORIGIN, "http://override.example");
    System.setProperty(McpAppOptions.KEY_ALLOWED_ORIGINS, "http://one.example, http://two.example");

    McpAppOptions options = McpAppOptions.load();

    assertEquals("http://override.example", options.getOrigin());
    assertEquals(List.of("http://one.example", "http://two.example"), options.getAllowedOrigins());
  }
}
