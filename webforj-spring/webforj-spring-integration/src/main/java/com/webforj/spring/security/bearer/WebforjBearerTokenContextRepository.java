package com.webforj.spring.security.bearer;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.oauth2.server.resource.authentication.AbstractOAuth2TokenAuthenticationToken;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.util.matcher.RequestMatcher;

/**
 * Keeps the login of a bearer token in the session, where webforJ reads it to check routes.
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenContextRepository extends HttpSessionSecurityContextRepository {
  private final RequestMatcher frameworkMatcher;

  WebforjBearerTokenContextRepository(RequestMatcher frameworkMatcher) {
    this.frameworkMatcher = frameworkMatcher;
  }

  /**
   * Saves the context in the session, also when its login is marked as transient. A new session
   * stores the login. A session that holds another login, such as a form login, is left unchanged.
   * A refreshed login of the same user is stored under the same session id.
   *
   * <p>
   * Any other login gets a new session id first. A webforJ framework request cannot change the
   * session id, so it stores nothing and the login holds for that request only.
   * </p>
   *
   * @param context the context to save
   * @param request the request whose session holds the context
   * @param response the response
   */
  @Override
  public void saveContext(SecurityContext context, HttpServletRequest request,
      HttpServletResponse response) {
    HttpSession session = request.getSession(false);
    if (session == null) {
      session = request.getSession();
    } else {
      Authentication stored =
          session.getAttribute(SPRING_SECURITY_CONTEXT_KEY) instanceof SecurityContext current
              ? current.getAuthentication()
              : null;
      if (stored != null && !(stored instanceof AbstractOAuth2TokenAuthenticationToken<?>)) {
        return;
      }

      if (!isSameUser(stored, context.getAuthentication())) {
        if (frameworkMatcher.matches(request)) {
          return;
        }

        request.changeSessionId();
      }
    }

    session.setAttribute(SPRING_SECURITY_CONTEXT_KEY, context);
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

  private boolean isSameUser(Authentication stored, Authentication login) {
    return stored != null && login != null && stored.getName().equals(login.getName());
  }
}
