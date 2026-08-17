package com.webforj.mcp;

import com.google.common.net.InetAddresses;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigValueFactory;
import com.webforj.mcp.filter.AuthorizationDiscoveryFilter;
import com.webforj.mcp.filter.CorsFilter;
import com.webforj.mcp.filter.FaviconFilter;
import com.webforj.router.RouteRegistry;
import com.webforj.servlet.WebforjServlet;
import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletRegistration;
import jakarta.servlet.SessionCookieConfig;
import java.lang.System.Logger;
import java.net.URI;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

/**
 * Assembles the MCP specifications a webforJ application contributes to a server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpAppContribution {

  private static final String LOCALHOST = "localhost";
  private static final Logger logger = System.getLogger(McpAppContribution.class.getName());
  private static final String CORS_FILTER_NAME = "webforjMcpCors";
  private static final String DISCOVERY_FILTER_NAME = "webforjMcpAuthorizationDiscovery";
  private static final String[] DISCOVERY_PATHS = {"/.well-known/oauth-protected-resource/*",
      "/.well-known/oauth-authorization-server/*", "/.well-known/openid-configuration/*"};
  private static final String COMPONENTS_KEY = "webforj.components";
  private static final String COMPONENTS_PATH = "/webapp/_lib/components";
  private static final String FAVICON_FILTER_NAME = "webforjMcpFavicon";
  private static final String FAVICON_PATH = "/favicon.ico";
  private static final String ICON_PATH = "/icons/icon-32x32.png";

  private final List<SyncToolSpecification> tools;
  private final List<McpAppResource> resources;
  private final McpAppOrigin origin;
  private final String servletPath;

  private McpAppContribution(List<SyncToolSpecification> tools, List<McpAppResource> resources,
      McpAppOrigin origin, String servletPath) {
    this.tools = List.copyOf(tools);
    this.resources = List.copyOf(resources);
    this.origin = origin;
    this.servletPath = servletPath;
  }

  /**
   * Assembles the contribution from the routes of the named packages.
   *
   * @param packages the packages the application routes live in
   * @param servletPath the mapping the webforJ servlet answers on, normalized internally
   * @return the contribution
   * @throws IllegalArgumentException if no package is named
   */
  public static McpAppContribution ofPackages(String[] packages, String servletPath) {
    if (packages == null || packages.length == 0) {
      throw new IllegalArgumentException(
          "Name the packages the application routes live in. The contribution never scans"
              + " the whole classpath.");
    }

    return ofRegistry(RouteRegistry.ofPackage(packages), servletPath);
  }

  /**
   * Assembles the contribution from a route registry the caller already holds.
   *
   * @param registry the registry holding the application routes
   * @param servletPath the mapping the webforJ servlet answers on, normalized internally
   * @return the contribution
   */
  public static McpAppContribution ofRegistry(RouteRegistry registry, String servletPath) {
    McpAppOrigin origin = new McpAppOrigin();
    String normalized = McpAppServletPath.normalize(servletPath);
    McpAppRegistry appRegistry = McpAppRegistry.ofRegistry(registry);

    // One resource per marked view, each serving the page that opens the application directly
    // at the route of its view, under the URI the tool of that route points at.
    List<McpAppResource> resources = appRegistry.getDescriptors().stream()
        .map(descriptor -> new McpAppResource(normalized, origin, descriptor.getRoute())).toList();

    return new McpAppContribution(appRegistry.getToolSpecifications(), resources, origin,
        normalized);
  }

  /**
   * Returns the tools the application contributes.
   *
   * @return the tool specifications, unmodifiable
   */
  public List<SyncToolSpecification> getToolSpecifications() {
    return tools;
  }

  /**
   * Returns the resources the application contributes.
   *
   * <p>
   * Configure the origin before handing the resources to a server, since the published policy names
   * the origin known at that moment.
   * </p>
   *
   * @return the resource specifications
   */
  public List<SyncResourceSpecification> getResourceSpecifications() {
    return resources.stream().map(McpAppResource::toSpecification).toList();
  }

  /**
   * Returns the origin resolution of this contribution.
   *
   * @return the origin resolution
   */
  public McpAppOrigin getOrigin() {
    return origin;
  }

  /**
   * Installs into the deployment everything a host needs to embed the application.
   *
   * <p>
   * The component library address is derived from the origin unless the deployment names one
   * itself, the embed filter answers cross origin requests, the session cookies carry the
   * attributes a cross site embed needs on a secure origin, the OAuth discovery paths answer not
   * found unless the application serves them with a servlet of its own, and the favicon answers
   * with the application icon. An application mounting the contribution on its server calls this
   * from its own initializer.
   * </p>
   *
   * @param context the servlet context of the deployment
   * @param options the options of the deployment
   */
  public void install(ServletContext context, McpAppOptions options) {
    declareDomains(options);
    deriveComponents(options);
    registerCorsFilter(context, options);
    registerAuthorizationDiscovery(context);
    registerFavicon(context);
    configureSessionCookies(context);
  }

  private void declareDomains(McpAppOptions options) {
    resources.forEach(resource -> resource.configureDomains(options.getResourceDomains(),
        options.getConnectDomains()));
  }

  private void deriveComponents(McpAppOptions options) {
    if (options.getComponents() != null) {
      // The deployment named its component library itself, an explicit value always wins.
      return;
    }

    String resolved = origin.resolve();
    if (resolved == null) {
      return;
    }

    String components = resolved + servletPath + COMPONENTS_PATH;
    Config derived =
        ConfigFactory.empty().withValue(COMPONENTS_KEY, ConfigValueFactory.fromAnyRef(components));
    Config existing = WebforjServlet.getInitConfig();
    // The servlet slot replaces on set, so the caller merges to keep what the slot carries.
    WebforjServlet.setConfig(existing == null ? derived : existing.withFallback(derived));

    logger.log(Logger.Level.INFO,
        () -> "webforJ component library derived from the origin: " + components);
  }

  private static void registerCorsFilter(ServletContext context, McpAppOptions options) {
    FilterRegistration.Dynamic cors =
        context.addFilter(CORS_FILTER_NAME, new CorsFilter(options.getAllowedOrigins()));
    if (cors != null) {
      // The filter sits in front of the MCP endpoint too, and a registration without async
      // support would refuse the streamed answers the endpoint sends.
      cors.setAsyncSupported(true);
      cors.addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), false, "/*");
    }
  }

  private static void registerAuthorizationDiscovery(ServletContext context) {
    String webforjServletName = findWebforjServletName(context);
    if (webforjServletName == null) {
      // Without the webforJ servlet nothing swallows unknown paths, so the container already
      // answers the discovery probes with not found.
      return;
    }

    FilterRegistration.Dynamic discovery = context.addFilter(DISCOVERY_FILTER_NAME,
        new AuthorizationDiscoveryFilter(webforjServletName));
    if (discovery == null) {
      logger.log(Logger.Level.WARNING, "A filter named " + DISCOVERY_FILTER_NAME
          + " is already registered, the authorization discovery paths keep their answers");
      return;
    }

    discovery.addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), true, DISCOVERY_PATHS);
  }

  private void registerFavicon(ServletContext context) {
    if (findWebforjServletName(context) == null) {
      // Without the webforJ servlet there is no icons endpoint to serve the icon from.
      return;
    }

    FilterRegistration.Dynamic favicon =
        context.addFilter(FAVICON_FILTER_NAME, new FaviconFilter(servletPath + ICON_PATH));
    if (favicon == null) {
      logger.log(Logger.Level.WARNING, "A filter named " + FAVICON_FILTER_NAME
          + " is already registered, the favicon keeps its answer");
      return;
    }

    favicon.addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), true, FAVICON_PATH);
  }

  private static String findWebforjServletName(ServletContext context) {
    for (ServletRegistration registration : context.getServletRegistrations().values()) {
      if (WebforjServlet.class.getName().equals(registration.getClassName())) {
        return registration.getName();
      }
    }

    return null;
  }

  private void configureSessionCookies(ServletContext context) {
    String resolved = origin.resolve();
    if (!supportsCrossSiteCookies(resolved)) {
      return;
    }

    SessionCookieConfig cookies = context.getSessionCookieConfig();
    cookies.setSecure(true);
    cookies.setAttribute("SameSite", "None");
    cookies.setAttribute("Partitioned", "");
  }

  private static boolean supportsCrossSiteCookies(String resolved) {
    if (resolved == null) {
      return false;
    }

    URI uri = URI.create(resolved);
    if ("https".equalsIgnoreCase(uri.getScheme())) {
      return true;
    }

    String host = uri.getHost();
    return "http".equalsIgnoreCase(uri.getScheme()) && isLoopback(host);
  }

  private static boolean isLoopback(String host) {
    String address = normalize(host);
    if (address == null) {
      return false;
    }

    if (LOCALHOST.equals(address)) {
      return true;
    }

    return InetAddresses.isInetAddress(address)
        && InetAddresses.forString(address).isLoopbackAddress();
  }

  private static String normalize(String host) {
    if (host == null) {
      return null;
    }

    String normalized = host.trim().toLowerCase(Locale.ROOT);
    if (normalized.startsWith("[") && normalized.endsWith("]")) {
      normalized = normalized.substring(1, normalized.length() - 1);
    }

    int zone = normalized.indexOf('%');
    if (zone >= 0) {
      normalized = normalized.substring(0, zone);
    }

    return normalized.isEmpty() ? null : normalized;
  }
}
