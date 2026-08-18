package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceRequest;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceResult;
import io.modelcontextprotocol.spec.McpSchema.TextResourceContents;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class McpAppResourceTest {

  private final McpAppOrigin origin = new McpAppOrigin();
  private final McpAppResource resource = new McpAppResource("", origin);

  @BeforeEach
  @AfterEach
  void clearOriginProperty() {
    System.clearProperty(McpAppOptions.KEY_ORIGIN);
  }

  @Test
  @DisplayName("Should refuse to render before an origin is known")
  void shouldRefuseWithoutOrigin() {
    IllegalStateException thrown = assertThrows(IllegalStateException.class, resource::render);

    assertTrue(thrown.getMessage().contains(McpAppOptions.KEY_ORIGIN));
  }

  @Test
  @DisplayName("Should address the embed bootstrap on the configured origin")
  void shouldAddressBootstrapOnOrigin() {
    origin.configure("https://app.example.com");

    String page = resource.render();

    assertTrue(page.contains("https://app.example.com/dwcembed/webforj.js"));
    assertTrue(page.contains("var context=\"https://app.example.com\""));
    assertFalse(page.contains("__WEBFORJ_ORIGIN__"));
    assertFalse(page.contains("__SERVLET_PATH__"));
    assertFalse(page.contains("__EMBED_LOCATION__"));
  }

  @Test
  @DisplayName("Should open the application at the route of its view")
  void shouldOpenAtItsRoute() {
    origin.configure("https://app.example.com");

    McpAppResource routed = new McpAppResource("", origin, "/inventory");

    assertTrue(routed.render().contains("__webforjEmbedLocation=\"/inventory\""));
    assertEquals("ui://webforj/app/inventory", routed.toSpecification().resource().uri());
  }

  @Test
  @DisplayName("Should open the application at the root for the root route")
  void shouldOpenAtRootByDefault() {
    origin.configure("https://app.example.com");

    assertTrue(resource.render().contains("__webforjEmbedLocation=\"/\""));
    assertEquals(McpAppResource.APP_RESOURCE_URI, resource.toSpecification().resource().uri());
  }

  @Test
  @DisplayName("Should let the system property win over the configured origin")
  void shouldPreferSystemProperty() {
    origin.configure("https://configured.example.com");
    System.setProperty(McpAppOptions.KEY_ORIGIN, "https://property.example.com");

    assertTrue(resource.render().contains("var context=\"https://property.example.com\""));
  }

  @Test
  @DisplayName("Should address the embed bootstrap under a remapped servlet")
  void shouldAddressBootstrapUnderPrefix() {
    origin.configure("https://app.example.com");

    String page = new McpAppResource("/ui", origin).render();

    assertTrue(page.contains("https://app.example.com/ui/dwcembed/webforj.js"));
  }

  @Test
  @DisplayName("Should carry the channel and boot the official SDK from the pinned address")
  void shouldCarryChannel() {
    origin.configure("https://app.example.com");

    String page = resource.render();

    assertTrue(page.contains("__webforjMcpChannel"));
    String sdkUrl = "https://cdn.jsdelivr.net/npm/@modelcontextprotocol/ext-apps@1.7.5/"
        + "dist/src/app-with-deps.js";
    assertTrue(page.contains(sdkUrl));
    assertTrue(page.contains(".App("));
    assertTrue(page.contains("__webforjMcpAttachFailed"));
    assertTrue(page.contains("availableDisplayModes"));
    assertTrue(page.contains("webforj-mcp-message"));
  }

  @Test
  @DisplayName("Should carry the credentials the cross site embed needs")
  void shouldCarryCredentials() {
    origin.configure("https://app.example.com");

    String page = resource.render();

    assertTrue(page.contains("use-credentials"));
    assertTrue(page.contains("withCredentials=!0"));
    assertTrue(page.contains("credentials=\"include\""));
  }

  @Test
  @DisplayName("Should publish the app resource as an interactive page")
  void shouldPublishInteractivePage() {
    origin.configure("https://app.example.com");

    SyncResourceSpecification specification = resource.toSpecification();

    assertEquals(McpAppResource.APP_RESOURCE_URI, specification.resource().uri());
    assertEquals(McpAppResource.MIME_TYPE, specification.resource().mimeType());

    ReadResourceResult result = specification.readHandler().apply(null,
        ReadResourceRequest.builder(McpAppResource.APP_RESOURCE_URI).build());
    TextResourceContents contents = (TextResourceContents) result.contents().get(0);

    assertEquals(McpAppResource.APP_RESOURCE_URI, contents.uri());
    assertTrue(contents.text().contains("bbj-dwc"));
  }

  @Test
  @DisplayName("Should publish a policy naming the origin and its socket")
  void shouldPublishPolicyMeta() {
    origin.configure("https://app.example.com");

    Map<String, Object> meta = resource.toSpecification().resource().meta();

    @SuppressWarnings("unchecked")
    Map<String, Object> ui = (Map<String, Object>) meta.get("ui");
    @SuppressWarnings("unchecked")
    Map<String, Object> csp = (Map<String, Object>) ui.get("csp");

    assertFalse(ui.containsKey("domain"),
        "hosts derive the sandbox domain themselves and refuse a published one");
    assertEquals(
        List.of("https://app.example.com", "https://cdn.jsdelivr.net", "https://www.gstatic.com"),
        csp.get("resourceDomains"));
    assertFalse(csp.containsKey("frameDomains"),
        "an application that embeds no frames must not ask hosts for frame permissions");
    assertEquals(List.of("https://app.example.com", "wss://app.example.com",
        "https://cdn.jsdelivr.net", "https://www.gstatic.com", "data:"), csp.get("connectDomains"));
  }

  @Test
  @DisplayName("Should publish the widget policy under both key forms")
  void shouldPublishWidgetPolicyMeta() {
    origin.configure("https://app.example.com");

    Map<String, Object> meta = resource.toSpecification().resource().meta();

    @SuppressWarnings("unchecked")
    Map<String, Object> widgetCsp = (Map<String, Object>) meta.get("openai/widgetCSP");

    assertEquals("https://app.example.com", meta.get("openai/widgetDomain"));
    assertEquals(
        List.of("https://app.example.com", "https://cdn.jsdelivr.net", "https://www.gstatic.com"),
        widgetCsp.get("resource_domains"));
    assertFalse(widgetCsp.containsKey("frame_domains"),
        "an application that embeds no frames must not ask hosts for frame permissions");
    assertEquals(List.of("https://app.example.com", "wss://app.example.com",
        "https://cdn.jsdelivr.net", "https://www.gstatic.com", "data:"),
        widgetCsp.get("connect_domains"));
  }

  @Test
  @DisplayName("Should carry the policy on the read contents with the origin known at read time")
  void shouldCarryPolicyOnReadContents() {
    SyncResourceSpecification specification = resource.toSpecification();
    origin.configure("https://late.example.com");

    ReadResourceResult result = specification.readHandler().apply(null,
        ReadResourceRequest.builder(McpAppResource.APP_RESOURCE_URI).build());
    Map<String, Object> meta = ((TextResourceContents) result.contents().get(0)).meta();

    assertEquals("https://late.example.com", meta.get("openai/widgetDomain"));

    @SuppressWarnings("unchecked")
    Map<String, Object> ui = (Map<String, Object>) meta.get("ui");
    @SuppressWarnings("unchecked")
    Map<String, Object> csp = (Map<String, Object>) ui.get("csp");
    assertEquals(
        List.of("https://late.example.com", "https://cdn.jsdelivr.net", "https://www.gstatic.com"),
        csp.get("resourceDomains"));
  }

  @Test
  @DisplayName("Should carry declared domains with the origin and framework domains")
  void shouldCarryDeclaredDomains() {
    origin.configure("https://app.example.com");
    resource.configureDomains(List.of("https://tiles.example.com", "https://cdn.jsdelivr.net"),
        List.of("https://api.example.com"));

    Map<String, Object> meta = resource.toSpecification().resource().meta();

    @SuppressWarnings("unchecked")
    Map<String, Object> ui = (Map<String, Object>) meta.get("ui");
    @SuppressWarnings("unchecked")
    Map<String, Object> csp = (Map<String, Object>) ui.get("csp");

    assertEquals(List.of("https://app.example.com", "https://cdn.jsdelivr.net",
        "https://www.gstatic.com", "https://tiles.example.com"), csp.get("resourceDomains"));
    assertEquals(
        List.of("https://app.example.com", "wss://app.example.com", "https://cdn.jsdelivr.net",
            "https://www.gstatic.com", "data:", "https://api.example.com"),
        csp.get("connectDomains"));
  }

  @Test
  @DisplayName("Should publish without a policy while no origin is known")
  void shouldPublishWithoutPolicyMeta() {
    Map<String, Object> meta = resource.toSpecification().resource().meta();

    assertTrue(meta == null || meta.isEmpty());
  }

  @Test
  @DisplayName("Should take the origin a request arrived on when none is configured")
  void shouldTakeObservedOrigin() {
    HttpServletRequest request = mock(HttpServletRequest.class);
    when(request.getScheme()).thenReturn("http");
    when(request.getServerName()).thenReturn("localhost");
    when(request.getServerPort()).thenReturn(8080);

    origin.observe(request);

    assertTrue(resource.render().contains("http://localhost:8080/dwcembed/webforj.js"));
  }
}
