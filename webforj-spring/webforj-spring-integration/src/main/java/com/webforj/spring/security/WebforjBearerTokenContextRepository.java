package com.webforj.spring.security;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.oauth2.server.resource.authentication.AbstractOAuth2TokenAuthenticationToken;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

/**
 * Keeps the login of a bearer token in the session, where webforJ reads it to check routes.
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenContextRepository extends HttpSessionSecurityContextRepository {

  /**
   * Saves the context in the session, also when its login is marked as transient.
   *
   * @param context the context to save
   * @param request the request whose session holds the context
   * @param response the response
   */
  @Override
  public void saveContext(SecurityContext context, HttpServletRequest request,
      HttpServletResponse response) {
    request.getSession().setAttribute(SPRING_SECURITY_CONTEXT_KEY, context);
  }

  /**
   * Removes a bearer token login from the session of the request. Any other login stays.
   *
   * @param request the request whose session holds the login
   */
  void removeContext(HttpServletRequest request) {
    HttpSession session = request.getSession(false);
    if (session != null
        && session.getAttribute(SPRING_SECURITY_CONTEXT_KEY) instanceof SecurityContext context
        && context.getAuthentication() instanceof AbstractOAuth2TokenAuthenticationToken<?>) {
      session.removeAttribute(SPRING_SECURITY_CONTEXT_KEY);
    }
  }
}
