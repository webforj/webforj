package com.webforj.spring.security;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletRequestWrapper;
import java.util.Arrays;
import java.util.Set;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.server.resource.web.BearerTokenResolver;
import org.springframework.security.oauth2.server.resource.web.DefaultBearerTokenResolver;
import org.springframework.security.web.util.matcher.RequestMatcher;
import org.springframework.util.StringUtils;
import org.springframework.web.util.WebUtils;

/**
 * Resolves the bearer token of a request from the authorization header or from a cookie.
 *
 * <p>
 * The cookie is read only when the request has no authorization header token, and only on read
 * requests or webforJ framework requests, so a cookie never exempts a request from CSRF protection.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenResolver implements BearerTokenResolver {
  private static final Set<String> COOKIE_METHODS = Set.of("GET", "HEAD", "OPTIONS");

  private final DefaultBearerTokenResolver headerResolver = new DefaultBearerTokenResolver();
  private final String cookieName;
  private final RequestMatcher frameworkMatcher;

  WebforjBearerTokenResolver(String cookieName, RequestMatcher frameworkMatcher) {
    this.cookieName = cookieName;
    this.frameworkMatcher = frameworkMatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(HttpServletRequest request) {
    String headerToken = headerResolver.resolve(request);
    if (headerToken != null || !isCookieReadable(request)) {
      return headerToken;
    }

    Cookie cookie = WebUtils.getCookie(request, cookieName);
    if (cookie == null || !StringUtils.hasText(cookie.getValue())) {
      return null;
    }

    return cookie.getValue();
  }

  /**
   * Checks whether the request carries a well formed token.
   *
   * @param request the request
   * @return true when a token is resolved, false when none is present or the header is malformed
   */
  boolean hasToken(HttpServletRequest request) {
    try {
      return resolve(request) != null;
    } catch (OAuth2AuthenticationException e) {
      return false;
    }
  }

  boolean hasHeaderToken(HttpServletRequest request) {
    return headerResolver.resolve(request) != null;
  }

  /**
   * Hides the token cookie of the request from the filters that follow.
   *
   * @param request the request
   * @return the request without the token cookie
   */
  HttpServletRequest hideCookieToken(HttpServletRequest request) {
    return new CookieTokenHidingRequest(request, cookieName);
  }

  private boolean isCookieReadable(HttpServletRequest request) {
    return COOKIE_METHODS.contains(request.getMethod()) || frameworkMatcher.matches(request);
  }

  private static final class CookieTokenHidingRequest extends HttpServletRequestWrapper {
    private final String cookieName;

    CookieTokenHidingRequest(HttpServletRequest request, String cookieName) {
      super(request);
      this.cookieName = cookieName;
    }

    @Override
    public Cookie[] getCookies() {
      return Arrays.stream(super.getCookies())
          .filter(cookie -> !cookie.getName().equals(cookieName)).toArray(Cookie[]::new);
    }
  }
}
