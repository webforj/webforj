package com.webforj.spring.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.security.core.context.SecurityContextHolderStrategy;
import org.springframework.security.oauth2.server.resource.authentication.AbstractOAuth2TokenAuthenticationToken;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * Keeps the stored bearer token login in step with the token of each request.
 *
 * <p>
 * A request without a token removes the stored bearer token login. A request whose cookie token was
 * rejected continues anonymously without the token, so that route security decides how to answer
 * it.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenFilter extends OncePerRequestFilter {
  private final WebforjBearerTokenResolver resolver;
  private final WebforjBearerTokenContextRepository repository;
  private final SecurityContextHolderStrategy holderStrategy;

  WebforjBearerTokenFilter(WebforjBearerTokenResolver resolver,
      WebforjBearerTokenContextRepository repository,
      SecurityContextHolderStrategy holderStrategy) {
    this.resolver = resolver;
    this.repository = repository;
    this.holderStrategy = holderStrategy;
  }

  @Override
  protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
      FilterChain filterChain) throws ServletException, IOException {
    if (holderStrategy.getContext()
        .getAuthentication() instanceof AbstractOAuth2TokenAuthenticationToken<?>) {
      holderStrategy.setContext(holderStrategy.createEmptyContext());
    }

    if (!resolver.hasToken(request)) {
      repository.removeContext(request);
      filterChain.doFilter(request, response);
      return;
    }

    filterChain.doFilter(request, response);
    if (request.getAttribute(WebforjBearerTokenFailureHandler.REJECTED_ATTRIBUTE_NAME) != null) {
      filterChain.doFilter(resolver.hideCookieToken(request), response);
    }
  }
}
