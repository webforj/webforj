package com.webforj.mcp.filter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import jakarta.servlet.FilterChain;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class FaviconFilterTest {

  private final FaviconFilter filter = new FaviconFilter("/icons/icon-32x32.png");
  private final HttpServletRequest request = mock(HttpServletRequest.class);
  private final HttpServletResponse response = mock(HttpServletResponse.class);
  private final FilterChain chain = mock(FilterChain.class);

  @Test
  @DisplayName("Should forward to the icons endpoint of the application")
  void shouldForwardToIconsEndpoint() throws IOException, ServletException {
    RequestDispatcher dispatcher = mock(RequestDispatcher.class);
    when(request.getRequestDispatcher("/icons/icon-32x32.png")).thenReturn(dispatcher);

    filter.doFilter(request, response, chain);

    verify(dispatcher).forward(request, response);
    verifyNoInteractions(chain);
  }
}
