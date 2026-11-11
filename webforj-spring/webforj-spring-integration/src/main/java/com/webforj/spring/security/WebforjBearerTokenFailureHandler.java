package com.webforj.spring.security;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.authentication.AuthenticationEntryPointFailureHandler;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;

/**
 * Handles a rejected bearer token.
 *
 * <p>
 * The stored bearer token login is removed. A rejected header token is answered by the
 * authentication entry point of the resource server. A rejected cookie token writes no response and
 * marks the request, so that it continues without the token. A failure of the token check itself is
 * rethrown and keeps the stored login.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenFailureHandler implements AuthenticationFailureHandler {
  static final String REJECTED_ATTRIBUTE_NAME =
      WebforjBearerTokenFailureHandler.class.getName() + ".REJECTED";

  private final AuthenticationFailureHandler headerFailureHandler;
  private final WebforjBearerTokenResolver resolver;
  private final WebforjBearerTokenContextRepository repository;

  WebforjBearerTokenFailureHandler(WebforjBearerTokenResolver resolver,
      WebforjBearerTokenContextRepository repository, AuthenticationEntryPoint entryPoint) {
    this.resolver = resolver;
    this.repository = repository;
    this.headerFailureHandler = new AuthenticationEntryPointFailureHandler(entryPoint);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
      AuthenticationException exception) throws IOException, ServletException {
    if (AuthenticationServiceException.class.isAssignableFrom(exception.getClass())) {
      throw exception;
    }

    repository.removeContext(request);
    if (resolver.hasHeaderToken(request)) {
      headerFailureHandler.onAuthenticationFailure(request, response, exception);
      return;
    }

    request.setAttribute(REJECTED_ATTRIBUTE_NAME, Boolean.TRUE);
  }
}
