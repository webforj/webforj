package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.servlet.WebforjServlet;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.McpSyncServer;
import io.modelcontextprotocol.server.transport.HttpServletStreamableServerTransportProvider;
import io.modelcontextprotocol.spec.McpSchema.ServerCapabilities;
import jakarta.servlet.DispatcherType;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletRegistration;
import jakarta.servlet.SessionCookieConfig;
import java.util.EnumSet;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class McpAppContributionTest {

  private static final String[] FIXTURE_PACKAGES = {"com.webforj.mcp.scanfixture"};

  @Test
  @DisplayName("Should contribute one resource per marked view under its route URI")
  void shouldContributeViewResource() {
    McpAppContribution contribution = McpAppContribution.ofPackages(FIXTURE_PACKAGES, "");

    assertTrue(contribution.getResourceSpecifications().stream()
        .anyMatch(specification -> (McpAppResource.APP_RESOURCE_URI + "/catalog")
            .equals(specification.resource().uri())));
  }

  @Test
  @DisplayName("Should contribute the view tools of the named packages")
  void shouldContributeViewTools() {
    McpAppContribution contribution = McpAppContribution.ofPackages(FIXTURE_PACKAGES, "");

    assertTrue(contribution.getToolSpecifications().stream()
        .anyMatch(specification -> "catalog".equals(specification.tool().name())));
  }

  @Test
  @DisplayName("Should refuse assembling without named packages")
  void shouldRefuseEmptyPackages() {
    assertThrows(IllegalArgumentException.class,
        () -> McpAppContribution.ofPackages(new String[0], ""));
    assertThrows(IllegalArgumentException.class, () -> McpAppContribution.ofPackages(null, ""));
  }

  @Test
  @DisplayName("Should mount on a server a developer assembles with the SDK")
  void shouldMountOnDeveloperAssembledServer() {
    McpAppContribution contribution = McpAppContribution.ofPackages(FIXTURE_PACKAGES, "");

    HttpServletStreamableServerTransportProvider transport =
        HttpServletStreamableServerTransportProvider.builder().mcpEndpoint("/mcp").build();
    McpSyncServer server = McpServer.sync(transport).serverInfo("developer-server", "1.0.0")
        .capabilities(ServerCapabilities.builder().tools(true).resources(false, true).build())
        .tools(contribution.getToolSpecifications())
        .resources(contribution.getResourceSpecifications()).build();

    try {
      assertNotNull(server);
    } finally {
      server.close();
    }
  }

  @Nested
  class Install {

    private final ServletContext context = mock(ServletContext.class);
    private final McpAppContribution contribution =
        McpAppContribution.ofPackages(FIXTURE_PACKAGES, "");

    @AfterEach
    void clearServletConfig() {
      WebforjServlet.setConfig(null);
    }

    @Test
    @DisplayName("Should front the deployment with the embed filter")
    void shouldRegisterEmbedFilter() {
      FilterRegistration.Dynamic cors = mock(FilterRegistration.Dynamic.class);
      when(context.addFilter(eq("webforjMcpCors"), any(Filter.class))).thenReturn(cors);

      contribution.install(context, new McpAppOptions());

      verify(cors).setAsyncSupported(true);
    }

    @Test
    @DisplayName("Should register no discovery filter without the webforJ servlet")
    void shouldSkipDiscoveryWithoutWebforjServlet() {
      contribution.install(context, new McpAppOptions());

      verify(context, never()).addFilter(eq("webforjMcpAuthorizationDiscovery"), any(Filter.class));
    }

    @Test
    @DisplayName("Should register no favicon filter without the webforJ servlet")
    void shouldSkipFaviconWithoutWebforjServlet() {
      contribution.install(context, new McpAppOptions());

      verify(context, never()).addFilter(eq("webforjMcpFavicon"), any(Filter.class));
    }

    @Test
    @DisplayName("Should answer the favicon from the icons endpoint of the application")
    void shouldRegisterFaviconFilter() {
      ServletRegistration webforj = mock(ServletRegistration.class);
      when(webforj.getClassName()).thenReturn(WebforjServlet.class.getName());
      when(webforj.getName()).thenReturn("WebforjServlet");
      doReturn(Map.of("WebforjServlet", webforj)).when(context).getServletRegistrations();
      FilterRegistration.Dynamic favicon = mock(FilterRegistration.Dynamic.class);
      when(context.addFilter(eq("webforjMcpFavicon"), any(Filter.class))).thenReturn(favicon);

      contribution.install(context, new McpAppOptions());

      verify(favicon).addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), true,
          "/favicon.ico");
    }

    @Test
    @DisplayName("Should mark the session cookies for a cross site embed on a secure origin")
    void shouldConfigureCookiesForSecureOrigin() {
      contribution.getOrigin().configure("https://app.example.com");
      SessionCookieConfig cookies = mock(SessionCookieConfig.class);
      when(context.getSessionCookieConfig()).thenReturn(cookies);

      contribution.install(context, new McpAppOptions());

      verify(cookies).setSecure(true);
      verify(cookies).setAttribute("SameSite", "None");
      verify(cookies).setAttribute("Partitioned", "");
    }

    @Test
    @DisplayName("Should keep the session cookie defaults on a plain http origin")
    void shouldKeepCookieDefaultsForPlainOrigin() {
      contribution.getOrigin().configure("http://localhost:8080");

      contribution.install(context, new McpAppOptions());

      verify(context, never()).getSessionCookieConfig();
    }
  }
}
