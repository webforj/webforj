package com.webforj.spring.security;

import static com.webforj.router.security.AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.RedirectStrategy;

@ExtendWith(MockitoExtension.class)
class WebforjAuthenticationSuccessHandlerTest {

  private WebforjAuthenticationSuccessHandler handler;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private HttpSession session;

  @Mock
  private Authentication authentication;

  @Mock
  private RedirectStrategy redirectStrategy;

  @BeforeEach
  void setUp() {
    handler = new WebforjAuthenticationSuccessHandler();
    handler.setRedirectStrategy(redirectStrategy);
  }

  @Nested
  class WebforjLocationRedirect {

    @Test
    void shouldRedirectToStoredWebforjLocation() throws IOException, ServletException {
      String storedLocation = "/protected/page";
      when(request.getSession()).thenReturn(session);
      when(session.getAttribute(PRE_AUTH_LOCATION_KEY)).thenReturn(storedLocation);

      handler.onAuthenticationSuccess(request, response, authentication);

      verify(session).removeAttribute(PRE_AUTH_LOCATION_KEY);
      verify(redirectStrategy).sendRedirect(request, response, storedLocation);
    }
  }

  @Nested
  class FallbackBehavior {

    @Test
    void shouldFallBackToDefaultWhenNoStoredLocation() throws IOException, ServletException {
      when(request.getSession()).thenReturn(session);
      when(session.getAttribute(PRE_AUTH_LOCATION_KEY)).thenReturn(null);

      WebforjAuthenticationSuccessHandler spyHandler = spy(handler);
      spyHandler.setRedirectStrategy(redirectStrategy);

      spyHandler.onAuthenticationSuccess(request, response, authentication);

      verify(session, never()).removeAttribute(PRE_AUTH_LOCATION_KEY);
    }

    @Test
    void shouldFallBackWhenNoSession() throws IOException, ServletException {
      when(request.getSession()).thenReturn(null);

      WebforjAuthenticationSuccessHandler spyHandler = spy(handler);
      spyHandler.setRedirectStrategy(redirectStrategy);

      spyHandler.onAuthenticationSuccess(request, response, authentication);

      verify(session, never()).removeAttribute(any());
    }
  }
}
