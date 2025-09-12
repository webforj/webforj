package com.webforj.spring.security;

import com.webforj.spring.SpringConfigurationProperties;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.web.util.matcher.RequestMatcher;

/**
 * Matcher for identifying webforJ framework requests that need CSRF exemption.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class WebforjFrameworkRequestMatcher implements RequestMatcher {

  // Core framework paths that need CSRF exemption
  private static final String RPC_PATH = "/webapprmi";
  private static final String WEBAPP_LIB = "/webapp/_lib/";

  // Default configurable paths
  private static final String DEFAULT_ICONS_DIR = "icons/";
  private static final String DEFAULT_ASSETS_DIR = "static/";

  private final SpringConfigurationProperties properties;
  private final String servletPrefix;

  /**
   * Creates a matcher with custom configuration.
   *
   * @param properties the webforJ configuration properties
   */
  public WebforjFrameworkRequestMatcher(SpringConfigurationProperties properties) {
    this.properties = properties;

    // Determine the servlet prefix based on the mapping
    String mapping = properties.getServletMapping();
    if ("/*".equals(mapping)) {
      // Root mapping uses internal /webforjServlet path
      this.servletPrefix = "/webforjServlet";
    } else if (mapping != null && mapping.endsWith("/*")) {
      // Extract prefix from mapping like "/app/*" -> "/app"
      this.servletPrefix = mapping.substring(0, mapping.length() - 2);
    } else {
      // No prefix for other mappings
      this.servletPrefix = "";
    }
  }

  /**
   * Checks if the request is a framework request that should be exempted from CSRF protection. This
   * includes RPC endpoints and resource paths that webforJ uses internally.
   *
   * @param request the HTTP request
   * @return true if this is a framework request that needs CSRF exemption
   */
  @Override
  public boolean matches(HttpServletRequest request) {
    return isFrameworkRequest(request);
  }

  /**
   * Checks if the request is a framework request that should be exempted from CSRF protection.
   *
   * @param request the HTTP request
   * @return true if this is a framework request that needs CSRF exemption
   */
  boolean isFrameworkRequest(HttpServletRequest request) {
    String path = getRequestPath(request);
    if (path == null) {
      return false;
    }

    // Check if this is a request under the servlet prefix
    String pathToCheck = path;
    if (!servletPrefix.isEmpty() && path.startsWith(servletPrefix)) {
      // Remove the servlet prefix to get the actual framework path
      pathToCheck = path.substring(servletPrefix.length());
    }

    // RPC endpoint - always allow
    if (pathToCheck.equals(RPC_PATH) || pathToCheck.startsWith(RPC_PATH + "/")) {
      return true;
    }

    // Library resources - always allow
    if (pathToCheck.startsWith(WEBAPP_LIB)) {
      return true;
    }

    // Icons directory - always allow
    String iconsDir = normalizeDirectory(properties.getIconsDir(), DEFAULT_ICONS_DIR);
    if (pathToCheck.startsWith(iconsDir)) {
      return true;
    }

    // Assets/static directory - always allow
    String assetsDir = normalizeDirectory(properties.getAssetsDir(), DEFAULT_ASSETS_DIR);
    if (pathToCheck.startsWith(assetsDir)) {
      return true;
    }

    return false;
  }

  /**
   * Normalizes a directory path to start with "/" and end with "/".
   */
  private String normalizeDirectory(String dir, String defaultDir) {
    if (dir == null || dir.isEmpty()) {
      dir = defaultDir;
    }

    // Ensure it starts with /
    if (!dir.startsWith("/")) {
      dir = "/" + dir;
    }

    // Ensure it ends with /
    if (!dir.endsWith("/")) {
      dir = dir + "/";
    }

    return dir;
  }

  /**
   * Gets the request path from servlet path or path info.
   *
   * @param request the HTTP request
   * @return the request path or null if not available
   */
  private String getRequestPath(HttpServletRequest request) {
    // Check if this is a forwarded request
    String forwardPath = (String) request.getAttribute("jakarta.servlet.forward.request_uri");
    if (forwardPath != null) {
      // This is a forwarded request, get the forward path
      String contextPath = request.getContextPath();
      if (contextPath != null && !contextPath.isEmpty() && forwardPath.startsWith(contextPath)) {
        return forwardPath.substring(contextPath.length());
      }
      return forwardPath;
    }

    // Get the full path including servlet path and path info
    String servletPath = request.getServletPath();
    String pathInfo = request.getPathInfo();

    String path;
    if (servletPath != null && !servletPath.isEmpty()) {
      path = servletPath;
      if (pathInfo != null && !pathInfo.isEmpty()) {
        path = path + pathInfo;
      }
    } else {
      // Fallback to request URI
      path = request.getRequestURI();
      // Remove context path if present
      String contextPath = request.getContextPath();
      if (contextPath != null && !contextPath.isEmpty() && path.startsWith(contextPath)) {
        path = path.substring(contextPath.length());
      }
    }

    return path;
  }
}
