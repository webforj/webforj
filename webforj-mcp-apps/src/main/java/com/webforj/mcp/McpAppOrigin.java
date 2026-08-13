package com.webforj.mcp;

import jakarta.servlet.http.HttpServletRequest;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Resolves the public origin the host loads the application from.
 *
 * <p>
 * A deployment behind a tunnel or a public domain configures the origin explicitly, since the
 * address the host must reach is not the address the container knows. A local run needs no
 * configuration because the origin of the requests arriving on the MCP endpoint is the origin the
 * host uses. The {@code webforj.origin} system property wins over the configured value, which wins
 * over the observed one.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpAppOrigin {

  private final AtomicReference<String> configured = new AtomicReference<>();
  private final AtomicReference<String> observed = new AtomicReference<>();

  /**
   * Returns the origin the app page loads the application from.
   *
   * @return the configured origin, or the origin last seen on the MCP endpoint, or {@code null}
   *         when neither is known yet
   */
  public String resolve() {
    String property = System.getProperty(McpAppOptions.KEY_ORIGIN);
    if (property != null && !property.isBlank()) {
      return trimTrailingSlashes(property.trim());
    }

    String wired = configured.get();
    if (wired != null) {
      return wired;
    }

    return observed.get();
  }

  /**
   * Sets the origin from application configuration.
   *
   * <p>
   * Integrations call this while wiring the application, so a deployment can carry the origin in
   * its own configuration instead of a JVM property. The JVM property still wins when both are
   * present.
   * </p>
   *
   * @param origin the public origin the host loads the application from
   */
  public void configure(String origin) {
    if (origin == null || origin.isBlank()) {
      return;
    }

    configured.set(trimTrailingSlashes(origin.trim()));
  }

  /**
   * Records the origin a request arrived on, so a local run needs no configuration.
   *
   * @param request the request that reached the MCP endpoint
   */
  public void observe(HttpServletRequest request) {
    if (request == null) {
      return;
    }

    StringBuilder origin =
        new StringBuilder(request.getScheme()).append("://").append(request.getServerName());
    int port = request.getServerPort();
    boolean defaultPort = ("http".equals(request.getScheme()) && port == 80)
        || ("https".equals(request.getScheme()) && port == 443);
    if (!defaultPort && port > 0) {
      origin.append(':').append(port);
    }

    observed.set(origin.toString());
  }

  private static String trimTrailingSlashes(String origin) {
    String trimmed = origin;
    while (trimmed.endsWith("/")) {
      trimmed = trimmed.substring(0, trimmed.length() - 1);
    }

    return trimmed;
  }
}
