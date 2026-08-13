package com.webforj.mcp.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Tells a probing host that the deployment requires no sign in.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class AuthorizationDiscoveryFilter extends HttpFilter {

  private final transient String webforjServletName;

  /**
   * Creates the filter for the deployment serving webforJ under the given servlet name.
   *
   * @param webforjServletName the name the webforJ servlet is registered under
   */
  public AuthorizationDiscoveryFilter(String webforjServletName) {
    this.webforjServletName = webforjServletName;
  }

  @Override
  protected void doFilter(HttpServletRequest request, HttpServletResponse response,
      FilterChain chain) throws IOException, ServletException {
    // The decision falls at request time, when the container has already picked the servlet by
    // most specific match. A discovery path the application claims with a servlet of its own
    // passes through untouched, whatever order the registrations happened in. Only a probe the
    // webforJ servlet would answer with the application page is turned into not found, the
    // signal that no authorization is required.
    if (request.getHttpServletMapping().getServletName().equals(webforjServletName)) {
      response.sendError(HttpServletResponse.SC_NOT_FOUND);
      return;
    }

    chain.doFilter(request, response);
  }
}
