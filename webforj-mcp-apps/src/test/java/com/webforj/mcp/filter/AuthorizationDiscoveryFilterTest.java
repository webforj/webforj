package com.webforj.mcp.filter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletMapping;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AuthorizationDiscoveryFilterTest {

  private final AuthorizationDiscoveryFilter filter =
      new AuthorizationDiscoveryFilter("WebforjServlet");
  private final HttpServletRequest request = mock(HttpServletRequest.class);
  private final HttpServletResponse response = mock(HttpServletResponse.class);
  private final FilterChain chain = mock(FilterChain.class);

  @Test
  @DisplayName("Should answer not found when the webforJ servlet would serve the probe")
  void shouldAnswerNotFoundForWebforjTarget() throws IOException, ServletException {
    mapRequestTo("WebforjServlet");

    filter.doFilter(request, response, chain);

    verify(response).sendError(HttpServletResponse.SC_NOT_FOUND);
    verifyNoInteractions(chain);
  }

  @Test
  @DisplayName("Should pass through to a servlet the application maps itself")
  void shouldPassThroughToApplicationServlet() throws IOException, ServletException {
    mapRequestTo("applicationMetadataServlet");

    filter.doFilter(request, response, chain);

    verify(chain).doFilter(request, response);
    verifyNoInteractions(response);
  }

  private void mapRequestTo(String servletName) {
    HttpServletMapping mapping = mock(HttpServletMapping.class);
    when(mapping.getServletName()).thenReturn(servletName);
    when(request.getHttpServletMapping()).thenReturn(mapping);
  }
}
