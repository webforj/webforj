package com.webforj.spring.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.mock.env.MockEnvironment;

class McpAppsEnvironmentPostProcessorTest {

  private final McpAppsEnvironmentPostProcessor processor = new McpAppsEnvironmentPostProcessor();

  @Test
  @DisplayName("Should exclude the default endpoint from the webforJ servlet")
  void shouldExcludeDefaultEndpoint() {
    MockEnvironment environment = new MockEnvironment();

    processor.postProcessEnvironment(environment, null);

    assertEquals("/mcp/**", environment.getProperty("webforj.exclude-urls[0]"));
  }

  @Test
  @DisplayName("Should exclude the authorization discovery paths from the webforJ servlet")
  void shouldExcludeDiscoveryPaths() {
    MockEnvironment environment = new MockEnvironment();

    processor.postProcessEnvironment(environment, null);

    assertEquals("/.well-known/oauth-protected-resource/**",
        environment.getProperty("webforj.exclude-urls[1]"));
    assertEquals("/.well-known/oauth-authorization-server/**",
        environment.getProperty("webforj.exclude-urls[2]"));
    assertEquals("/.well-known/openid-configuration/**",
        environment.getProperty("webforj.exclude-urls[3]"));
  }

  @Test
  @DisplayName("Should exclude the endpoint the application configures")
  void shouldExcludeConfiguredEndpoint() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("spring.ai.mcp.server.mcp-endpoint", "/assistant");

    processor.postProcessEnvironment(environment, null);

    assertEquals("/assistant/**", environment.getProperty("webforj.exclude-urls[0]"));
  }

  @Test
  @DisplayName("Should keep the exclusions the application already carries as a comma list")
  void shouldKeepCommaListExclusions() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.exclude-urls", "/api/**, /static/**");

    processor.postProcessEnvironment(environment, null);

    assertEquals("/api/**", environment.getProperty("webforj.exclude-urls[0]"));
    assertEquals("/static/**", environment.getProperty("webforj.exclude-urls[1]"));
    assertEquals("/mcp/**", environment.getProperty("webforj.exclude-urls[2]"));
  }

  @Test
  @DisplayName("Should keep the exclusions the application already carries as an indexed list")
  void shouldKeepIndexedExclusions() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.exclude-urls[0]", "/api/**");

    processor.postProcessEnvironment(environment, null);

    assertEquals("/api/**", environment.getProperty("webforj.exclude-urls[0]"));
    assertEquals("/mcp/**", environment.getProperty("webforj.exclude-urls[1]"));
  }

  @Test
  @DisplayName("Should add nothing when every default is already excluded")
  void shouldAddNothingWhenAlreadyExcluded() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.exclude-urls[0]", "/mcp/**");
    environment.setProperty("webforj.exclude-urls[1]", "/.well-known/oauth-protected-resource/**");
    environment.setProperty("webforj.exclude-urls[2]",
        "/.well-known/oauth-authorization-server/**");
    environment.setProperty("webforj.exclude-urls[3]", "/.well-known/openid-configuration/**");

    processor.postProcessEnvironment(environment, null);

    assertNull(environment.getProperty("webforj.exclude-urls[4]"));
  }

  @Test
  @DisplayName("Should derive the component library from the origin on a root mapped deployment")
  void shouldDeriveComponentsFromOrigin() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.origin", "http://localhost:8090");

    processor.postProcessEnvironment(environment, null);

    assertEquals("http://localhost:8090/webforjServlet/webapp/_lib/components",
        environment.getProperty("webforj.components"));
  }

  @Test
  @DisplayName("Should derive the component library under the mapping the application configures")
  void shouldDeriveComponentsUnderConfiguredMapping() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.origin", "http://localhost:8090/");
    environment.setProperty("webforj.servlet-mapping", "/ui/*");

    processor.postProcessEnvironment(environment, null);

    assertEquals("http://localhost:8090/ui/webapp/_lib/components",
        environment.getProperty("webforj.components"));
  }

  @Test
  @DisplayName("Should keep the component library the application declares itself")
  void shouldKeepDeclaredComponents() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("webforj.origin", "http://localhost:8090");
    environment.setProperty("webforj.components", "https://cdn.example/components");

    processor.postProcessEnvironment(environment, null);

    assertEquals("https://cdn.example/components", environment.getProperty("webforj.components"));
  }

  @Test
  @DisplayName("Should derive no component library without an origin")
  void shouldDeriveNothingWithoutOrigin() {
    MockEnvironment environment = new MockEnvironment();

    processor.postProcessEnvironment(environment, null);

    assertNull(environment.getProperty("webforj.components"));
  }
}
