package com.webforj.mcp;

import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceResult;
import io.modelcontextprotocol.spec.McpSchema.Resource;
import io.modelcontextprotocol.spec.McpSchema.TextResourceContents;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Serves the page that boots the application inside a host.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpAppResource {

  /**
   * The URI the resource of the root route is published under. Every other route publishes under
   * this URI extended by its route path.
   */
  static final String APP_RESOURCE_URI = "ui://webforj/app";

  /**
   * The media type that tells a host the resource is an interactive application.
   */
  static final String MIME_TYPE = "text/html;profile=mcp-app";

  private static final String TEMPLATE = "/META-INF/mcp/app.min.html";
  private static final String ROOT_ROUTE = "/";
  private static final String ORIGIN_PLACEHOLDER = "__WEBFORJ_ORIGIN__";
  private static final String SERVLET_PATH_PLACEHOLDER = "__SERVLET_PATH__";
  private static final String EMBED_LOCATION_PLACEHOLDER = "__EMBED_LOCATION__";

  // Domains required by framework resources.
  private static final List<String> FRAMEWORK_RESOURCE_DOMAINS =
      List.of("https://cdn.jsdelivr.net", "https://www.gstatic.com");

  // Domains and schemes required by framework connections.
  private static final List<String> FRAMEWORK_CONNECT_DOMAINS =
      List.of("https://cdn.jsdelivr.net", "https://www.gstatic.com", "data:");
  private static final String RESOURCE_NAME = "webforJ app";
  private static final String UI_KEY = "ui";
  private static final String CSP_KEY = "csp";
  private static final String WIDGET_CSP_KEY = "openai/widgetCSP";
  private static final String WIDGET_DOMAIN_KEY = "openai/widgetDomain";

  private final String servletPath;
  private final McpAppOrigin origin;
  private final String route;
  private final AtomicReference<List<String>> resourceDomains = new AtomicReference<>(List.of());
  private final AtomicReference<List<String>> connectDomains = new AtomicReference<>(List.of());

  /**
   * Creates the resource of the root route for a deployment serving webforJ under the given prefix.
   *
   * @param servletPath the prefix the webforJ servlet answers on, empty for the root
   * @param origin the origin resolution of the deployment
   */
  public McpAppResource(String servletPath, McpAppOrigin origin) {
    this(servletPath, origin, ROOT_ROUTE);
  }

  /**
   * Creates the resource of one view route for a deployment serving webforJ under the given prefix.
   *
   * @param servletPath the prefix the webforJ servlet answers on, empty for the root
   * @param origin the origin resolution of the deployment
   * @param route the route the served page opens the application at
   */
  public McpAppResource(String servletPath, McpAppOrigin origin, String route) {
    this.servletPath = servletPath == null ? "" : servletPath;
    this.origin = origin;
    this.route = route == null || route.isBlank() ? ROOT_ROUTE : route;
  }

  /**
   * Returns the URI the resource of the given route is published under.
   *
   * @param route the route a view is registered under
   * @return the resource URI the tool of that route points at
   */
  public static String getUriOf(String route) {
    if (route == null || route.isBlank() || ROOT_ROUTE.equals(route)) {
      return APP_RESOURCE_URI;
    }

    return APP_RESOURCE_URI + route;
  }

  /**
   * Returns the specification a server publishes for the app resource.
   *
   * @return the resource specification
   */
  public SyncResourceSpecification toSpecification() {
    String uri = getUriOf(route);
    String name = ROOT_ROUTE.equals(route) ? RESOURCE_NAME : RESOURCE_NAME + " " + route;
    Resource resource = Resource.builder(uri, name).description("The webforJ application")
        .mimeType(MIME_TYPE).meta(resourceMeta()).build();

    return new SyncResourceSpecification(resource,
        (exchange,
            request) -> ReadResourceResult.builder(List.of(TextResourceContents
                .builder(request.uri(), render()).mimeType(MIME_TYPE).meta(resourceMeta()).build()))
                .build());
  }

  /**
   * Renders the page against the origin the host reaches the application on.
   *
   * @return the page
   * @throws IllegalStateException if no origin is known yet
   */
  public String render() {
    String resolved = origin.resolve();
    if (resolved == null) {
      throw new IllegalStateException("No origin is known for the webforJ MCP app. Set "
          + McpAppOptions.KEY_ORIGIN + " to the address the host reaches the application on.");
    }

    return Template.CONTENT.replace(ORIGIN_PLACEHOLDER, resolved)
        .replace(SERVLET_PATH_PLACEHOLDER, servletPath).replace(EMBED_LOCATION_PLACEHOLDER, route);
  }

  void configureDomains(List<String> resourceDomains, List<String> connectDomains) {
    this.resourceDomains.set(resourceDomains == null ? List.of() : List.copyOf(resourceDomains));
    this.connectDomains.set(connectDomains == null ? List.of() : List.copyOf(connectDomains));
  }

  private Map<String, Object> resourceMeta() {
    String resolved = origin.resolve();
    if (resolved == null) {
      return Map.of();
    }

    return Map.of(UI_KEY, Map.of(CSP_KEY, contentSecurityPolicy(resolved)), WIDGET_CSP_KEY,
        widgetContentSecurityPolicy(resolved), WIDGET_DOMAIN_KEY, resolved);
  }

  private Map<String, Object> contentSecurityPolicy(String origin) {
    String socketOrigin = origin.replaceFirst("^http", "ws");

    return Map.of("resourceDomains",
        merge(List.of(origin), FRAMEWORK_RESOURCE_DOMAINS, resourceDomains.get()), "connectDomains",
        merge(List.of(origin, socketOrigin), FRAMEWORK_CONNECT_DOMAINS, connectDomains.get()));
  }

  private Map<String, Object> widgetContentSecurityPolicy(String origin) {
    String socketOrigin = origin.replaceFirst("^http", "ws");

    return Map.of("resource_domains",
        merge(List.of(origin), FRAMEWORK_RESOURCE_DOMAINS, resourceDomains.get()),
        "connect_domains",
        merge(List.of(origin, socketOrigin), FRAMEWORK_CONNECT_DOMAINS, connectDomains.get()));
  }

  private static List<String> merge(List<String> origins, List<String> framework,
      List<String> declared) {
    Set<String> merged = new LinkedHashSet<>(origins);
    merged.addAll(framework);
    merged.addAll(declared);

    return List.copyOf(merged);
  }

  private static final class Template {
    private static final String CONTENT = readResource(TEMPLATE);

    private Template() {
      // Constant holder
    }

    private static String readResource(String path) {
      try (InputStream stream = McpAppResource.class.getResourceAsStream(path)) {
        if (stream == null) {
          throw new IllegalStateException("The app page is missing from the jar at " + path);
        }

        return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
      } catch (IOException e) {
        throw new UncheckedIOException("Cannot read the app page at " + path, e);
      }
    }
  }
}
