package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.servlet.http.Cookie;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.oauth2.server.resource.InvalidBearerTokenException;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

class WebforjBearerTokenFailureHandlerTest {

  private static final String KEY =
      HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY;

  private final WebforjBearerTokenResolver resolver =
      new WebforjBearerTokenResolver("jwt", request -> false);
  private final WebforjBearerTokenFailureHandler handler = new WebforjBearerTokenFailureHandler(
      resolver, new WebforjBearerTokenContextRepository(request -> false));

  @ParameterizedTest
  @MethodSource("rejectedRequests")
  void shouldRemoveStoredBearerLogin(MockHttpServletRequest request) throws Exception {
    MockHttpSession session = new MockHttpSession();
    session.setAttribute(KEY,
        new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
    request.setSession(session);

    handler.onAuthenticationFailure(request, new MockHttpServletResponse(),
        new InvalidBearerTokenException("expired"));

    assertNull(session.getAttribute(KEY));
  }

  @Test
  void shouldMarkRejectedCookieTokenWithoutResponse() throws Exception {
    MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
    request.setCookies(new Cookie("jwt", "expired-token"));
    MockHttpServletResponse response = new MockHttpServletResponse();

    handler.onAuthenticationFailure(request, response, new InvalidBearerTokenException("expired"));

    assertEquals(Boolean.TRUE,
        request.getAttribute(WebforjBearerTokenFailureHandler.REJECTED_ATTRIBUTE_NAME));
    assertEquals(200, response.getStatus());
    assertNull(response.getHeader(HttpHeaders.WWW_AUTHENTICATE));
  }

  @Test
  void shouldAnswerRejectedHeaderTokenWithUnauthorized() throws Exception {
    MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
    request.addHeader(HttpHeaders.AUTHORIZATION, "Bearer expired-token");
    MockHttpServletResponse response = new MockHttpServletResponse();

    handler.onAuthenticationFailure(request, response, new InvalidBearerTokenException("expired"));

    assertEquals(401, response.getStatus());
    assertTrue(response.getHeader(HttpHeaders.WWW_AUTHENTICATE).startsWith("Bearer"));
    assertNull(request.getAttribute(WebforjBearerTokenFailureHandler.REJECTED_ATTRIBUTE_NAME));
  }

  private static Stream<Arguments> rejectedRequests() {
    MockHttpServletRequest cookieRequest = new MockHttpServletRequest("GET", "/account");
    cookieRequest.setCookies(new Cookie("jwt", "expired-token"));
    MockHttpServletRequest headerRequest = new MockHttpServletRequest("GET", "/account");
    headerRequest.addHeader(HttpHeaders.AUTHORIZATION, "Bearer expired-token");
    return Stream.of(Arguments.of(cookieRequest), Arguments.of(headerRequest));
  }
}
