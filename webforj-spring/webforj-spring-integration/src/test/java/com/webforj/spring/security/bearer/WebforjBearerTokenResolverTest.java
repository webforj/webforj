package com.webforj.spring.security.bearer;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import java.util.stream.Stream;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.web.util.matcher.RequestMatcher;

class WebforjBearerTokenResolverTest {

  private final RequestMatcher frameworkMatcher =
      request -> "/webapprmi".equals(request.getRequestURI());
  private final WebforjBearerTokenResolver resolver =
      new WebforjBearerTokenResolver("jwt", frameworkMatcher);

  @Nested
  class Resolving {

    @Test
    void shouldResolveHeaderTokenBeforeCookieToken() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.addHeader(HttpHeaders.AUTHORIZATION, "Bearer header-token");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertEquals("header-token", resolver.resolve(request));
    }

    @ParameterizedTest
    @ValueSource(strings = {"GET", "HEAD", "OPTIONS"})
    void shouldResolveCookieTokenOnReadMethods(String method) {
      MockHttpServletRequest request = new MockHttpServletRequest(method, "/account");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertEquals("cookie-token", resolver.resolve(request));
    }

    @ParameterizedTest
    @ValueSource(strings = {"POST", "PUT", "PATCH", "DELETE"})
    void shouldIgnoreCookieTokenOnWriteMethods(String method) {
      MockHttpServletRequest request = new MockHttpServletRequest(method, "/account");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertNull(resolver.resolve(request));
    }

    @Test
    void shouldResolveCookieTokenOnFrameworkRequest() {
      MockHttpServletRequest request = new MockHttpServletRequest("POST", "/webapprmi");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertEquals("cookie-token", resolver.resolve(request));
    }

    @ParameterizedTest
    @MethodSource("cookiesWithoutToken")
    void shouldResolveNothingWithoutTokenCookie(Cookie[] cookies) {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setCookies(cookies);

      assertNull(resolver.resolve(request));
    }

    private static Stream<Arguments> cookiesWithoutToken() {
      return Stream.of(
          Arguments.of(Named.of("other cookie", new Cookie[] {new Cookie("theme", "dark")})),
          Arguments.of(Named.of("no cookies", new Cookie[0])),
          Arguments.of(Named.of("blank token cookie", new Cookie[] {new Cookie("jwt", "")})));
    }
  }

  @Nested
  class TokenPresence {

    @Test
    void shouldHaveTokenWithTokenCookie() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertTrue(resolver.hasToken(request));
    }

    @Test
    void shouldHaveNoTokenWithoutTokenCookie() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");

      assertFalse(resolver.hasToken(request));
    }

    @Test
    void shouldHaveNoTokenWithMalformedHeader() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.addHeader(HttpHeaders.AUTHORIZATION, "Bearer @@@");

      assertFalse(resolver.hasToken(request));
    }

    @Test
    void shouldHaveHeaderTokenWithAuthorizationHeader() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.addHeader(HttpHeaders.AUTHORIZATION, "Bearer header-token");

      assertTrue(resolver.hasHeaderToken(request));
    }

    @Test
    void shouldHaveNoHeaderTokenWithTokenCookieOnly() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setCookies(new Cookie("jwt", "cookie-token"));

      assertFalse(resolver.hasHeaderToken(request));
    }
  }

  @Nested
  class Hiding {

    @Test
    void shouldHideTokenCookieAndKeepOtherCookies() {
      Cookie theme = new Cookie("theme", "dark");
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setCookies(new Cookie("jwt", "cookie-token"), theme);

      HttpServletRequest hidden = resolver.hideCookieToken(request);

      assertArrayEquals(new Cookie[] {theme}, hidden.getCookies());
      assertNull(resolver.resolve(hidden));
    }
  }
}
