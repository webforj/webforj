package com.webforj.mcp.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Serves the application icon as the favicon of the deployment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FaviconFilter extends HttpFilter {

  private final transient String iconPath;

  /**
   * Creates the filter forwarding the favicon to the icons endpoint of the application.
   *
   * @param iconPath the icons endpoint path the favicon forwards to
   */
  public FaviconFilter(String iconPath) {
    this.iconPath = iconPath;
  }

  @Override
  protected void doFilter(HttpServletRequest request, HttpServletResponse response,
      FilterChain chain) throws IOException, ServletException {
    // The favicon is the icon the application declares, served through the icons endpoint, so the
    // browser tab and the connector list of a host show the same image.
    request.getRequestDispatcher(iconPath).forward(request, response);
  }
}
