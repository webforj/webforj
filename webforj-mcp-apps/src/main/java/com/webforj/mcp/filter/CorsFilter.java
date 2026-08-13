package com.webforj.mcp.filter;

import com.webforj.mcp.McpAppOptions;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Answers the cross origin requests an embedded application makes back to its own server.
 *
 * <p>
 * Only the sandbox origins of the known MCP hosts are answered. A deployment extends them through
 * {@value McpAppOptions#KEY_ALLOWED_ORIGINS}, as exact origins or {@code https://*.suffix}
 * patterns. Every other origin is refused. The component library is the one exception: it is public
 * static content, answered for every origin without credentials.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CorsFilter implements Filter {

  private static final String ORIGIN = "Origin";
  private static final String ALLOWED_METHODS = "GET, POST, DELETE, OPTIONS";
  private static final String COMPONENTS_PATH = "/webapp/_lib/components/";
  private static final List<String> HOST_ORIGINS =
      List.of("https://*.claudemcpcontent.com", "https://*.claudeusercontent.com",
          "https://*.oaiusercontent.com", "codex-sandbox://*.oaiusercontent.com");
  private final List<String> allowedOrigins;

  /**
   * Creates a filter reading the allowed origins from the deployment configuration.
   */
  public CorsFilter() {
    this(McpAppOptions.load().getAllowedOrigins());
  }

  /**
   * Creates a filter answering the origins in a comma separated list beside the known hosts.
   *
   * @param allowedOrigins the additional origins allowed to embed the application, blank adding
   *        none
   */
  public CorsFilter(String allowedOrigins) {
    this(parseAllowedOrigins(allowedOrigins));
  }

  /**
   * Creates a filter answering the given origins beside the known hosts.
   *
   * @param allowedOrigins the additional origins allowed to embed the application, an empty list
   *        adding none
   */
  public CorsFilter(List<String> allowedOrigins) {
    this.allowedOrigins = List.copyOf(allowedOrigins);
  }

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
      throws IOException, ServletException {
    if (!(request instanceof HttpServletRequest httpRequest)
        || !(response instanceof HttpServletResponse httpResponse)) {
      chain.doFilter(request, response);
      return;
    }

    String origin = httpRequest.getHeader(ORIGIN);
    if (origin == null || origin.isBlank()) {
      chain.doFilter(request, response);
      return;
    }

    String path = httpRequest.getRequestURI();
    if (path != null && path.contains(COMPONENTS_PATH)) {
      // The component library is versioned public content, identical for every caller and free
      // of session data, so it is served the way a CDN serves it: any origin reads it and no
      // credentials ride the answer.
      httpResponse.setHeader("Access-Control-Allow-Origin", "*");
      if ("OPTIONS".equalsIgnoreCase(httpRequest.getMethod())) {
        httpResponse.setHeader("Access-Control-Allow-Methods", "GET, OPTIONS");
        httpResponse.setStatus(HttpServletResponse.SC_NO_CONTENT);
        return;
      }

      chain.doFilter(request, response);
      return;
    }

    if (!isAllowed(origin)) {
      if ("OPTIONS".equalsIgnoreCase(httpRequest.getMethod())) {
        httpResponse.setStatus(HttpServletResponse.SC_FORBIDDEN);
        return;
      }

      chain.doFilter(request, response);
      return;
    }

    httpResponse.setHeader("Access-Control-Allow-Origin", origin);
    httpResponse.setHeader("Access-Control-Allow-Credentials", "true");
    httpResponse.setHeader("Vary", ORIGIN);
    httpResponse.setHeader("Access-Control-Expose-Headers", "Mcp-Session-Id");

    if ("OPTIONS".equalsIgnoreCase(httpRequest.getMethod())) {
      httpResponse.setHeader("Access-Control-Allow-Methods", ALLOWED_METHODS);
      String requestedHeaders = httpRequest.getHeader("Access-Control-Request-Headers");
      httpResponse.setHeader("Access-Control-Allow-Headers",
          requestedHeaders == null ? "Content-Type" : requestedHeaders);
      httpResponse.setStatus(HttpServletResponse.SC_NO_CONTENT);
      return;
    }

    chain.doFilter(request, response);
  }

  private boolean isAllowed(String origin) {
    for (String pattern : HOST_ORIGINS) {
      if (matches(pattern, origin)) {
        return true;
      }
    }

    for (String pattern : allowedOrigins) {
      if (matches(pattern, origin)) {
        return true;
      }
    }

    return false;
  }

  private static boolean matches(String pattern, String origin) {
    int wildcard = pattern.indexOf('*');
    if (wildcard < 0) {
      return pattern.equals(origin);
    }

    String prefix = pattern.substring(0, wildcard);
    String suffix = pattern.substring(wildcard + 1);
    if (origin.length() <= prefix.length() + suffix.length() || !origin.startsWith(prefix)
        || !origin.endsWith(suffix)) {
      return false;
    }

    // The wildcard stands for host labels only, so an origin whose middle carries anything a
    // host name cannot is refused.
    String middle = origin.substring(prefix.length(), origin.length() - suffix.length());
    return middle.chars()
        .allMatch(character -> character == '-' || character == '.'
            || (character >= '0' && character <= '9') || (character >= 'a' && character <= 'z')
            || (character >= 'A' && character <= 'Z'));
  }

  private static List<String> parseAllowedOrigins(String value) {
    if (value == null || value.isBlank()) {
      return List.of();
    }

    List<String> origins = new ArrayList<>();
    for (String entry : value.split(",")) {
      String trimmed = entry.trim();
      if (!trimmed.isEmpty()) {
        origins.add(trimmed);
      }
    }

    return origins;
  }
}
