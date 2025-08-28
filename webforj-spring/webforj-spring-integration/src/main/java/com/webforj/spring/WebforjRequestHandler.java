package com.webforj.spring;

import jakarta.servlet.http.HttpServletRequest;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Collections;
import java.util.List;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.handler.SimpleUrlHandlerMapping;
import org.springframework.web.servlet.mvc.Controller;
import org.springframework.web.util.UrlPathHelper;

/**
 * Handler for routing requests between Spring MVC and webforJ.
 *
 * <p>
 * This handler implements the logic to determine whether a request should be handled by Spring MVC
 * controllers or forwarded to the webforJ servlet. It checks incoming requests against the
 * configured excluded URL patterns.
 * </p>
 *
 * <p>
 * Requests matching excluded URL patterns are handled by Spring MVC, while all other requests are
 * forwarded to the webforJ servlet.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
class WebforjRequestHandler extends SimpleUrlHandlerMapping {
  private final List<String> excludeUrls;
  private final PathMatcher pathMatcher = new AntPathMatcher();
  private final UrlPathHelper urlPathHelper = new UrlPathHelper();
  private static final Logger LOG = System.getLogger(WebforjRequestHandler.class.getName());

  /**
   * Constructs a new request handler.
   *
   * @param excludeUrls URL patterns to exclude from webforJ handling
   * @param forwardingController the controller that forwards to webforJ servlet
   * @param resourceHandlerMapping Spring's resource handler mapping
   */
  public WebforjRequestHandler(List<String> excludeUrls, Controller forwardingController,
      HandlerMapping resourceHandlerMapping) {
    this.excludeUrls = excludeUrls;

    // Set HIGH priority to intercept requests before RequestMappingHandlerMapping
    // RequestMappingHandlerMapping typically has order 0, so we use -1
    setOrder(-1);

    // Map all URLs to the forwarding controller by default
    setUrlMap(Collections.singletonMap("/**", forwardingController));

    LOG.log(Level.DEBUG, "Initialized WebforjRequestHandler with "
        + (excludeUrls != null ? excludeUrls.size() : 0) + " excluded URL patterns");
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Checks if the request matches any excluded URL patterns. If it does, returns {@code null} to
   * let Spring MVC handle it. Otherwise, forwards the request to the webforJ servlet.
   * </p>
   */
  @Override
  protected Object getHandlerInternal(HttpServletRequest request) throws Exception {
    String requestPath = urlPathHelper.getPathWithinApplication(request);

    if (isPathExcluded(requestPath)) {
      LOG.log(Level.TRACE,
          "Request to " + requestPath + " matches excluded pattern, " + "delegating to Spring MVC");
      return null; // Let Spring MVC handle it
    }

    LOG.log(Level.TRACE, "Request to " + requestPath + " will be forwarded to webforJ");

    // Forward to webforJ servlet
    return super.getHandlerInternal(request);
  }

  /**
   * Checks if a request path matches any of the excluded URL patterns. This method is
   * package-private for testing purposes.
   *
   * @param requestPath the path to check
   * @return true if the path is excluded, false otherwise
   */
  boolean isPathExcluded(String requestPath) {
    if (excludeUrls != null && !excludeUrls.isEmpty()) {
      for (String pattern : excludeUrls) {
        if (pathMatcher.match(pattern, requestPath)) {
          LOG.log(Level.TRACE, "Path " + requestPath + " matches excluded pattern " + pattern);
          return true;
        }
      }
    }
    return false;
  }
}
