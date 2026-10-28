package com.webforj.spring.security;

import com.webforj.environment.SessionObjectTable;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Optional;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.security.web.csrf.DeferredCsrfToken;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * Keeps the CSRF token of the current request in the session so that webforJ code can submit it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class WebforjCsrfTokenFilter extends OncePerRequestFilter {
  static final String SESSION_ATTRIBUTE_NAME =
      WebforjCsrfTokenFilter.class.getName() + ".CSRF_TOKEN";

  /**
   * Finds the CSRF token kept in the session of the current environment.
   *
   * @return the token, empty when no session or no token is available
   */
  static Optional<CsrfToken> findToken() {
    if (SessionObjectTable.contains(SESSION_ATTRIBUTE_NAME)
        && SessionObjectTable.get(SESSION_ATTRIBUTE_NAME) instanceof CsrfToken token) {
      return Optional.of(token);
    }

    return Optional.empty();
  }

  @Override
  protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
      FilterChain filterChain) throws ServletException, IOException {
    if (request
        .getAttribute(DeferredCsrfToken.class.getName()) instanceof DeferredCsrfToken deferred
        && request.getAttribute(CsrfToken.class.getName()) instanceof CsrfToken token) {
      String source = deferred.get().getToken();
      HttpSession session = request.getSession();
      if (!(session.getAttribute(SESSION_ATTRIBUTE_NAME) instanceof SessionCsrfToken stored)
          || !stored.getSource().equals(source)) {
        session.setAttribute(SESSION_ATTRIBUTE_NAME, new SessionCsrfToken(token, source));
      }
    }

    filterChain.doFilter(request, response);
  }

  /**
   * The token as the browser must submit it, together with the session token it was derived from.
   */
  private static final class SessionCsrfToken implements CsrfToken {
    private final String headerName;
    private final String parameterName;
    private final String token;
    private final String source;

    SessionCsrfToken(CsrfToken token, String source) {
      this.headerName = token.getHeaderName();
      this.parameterName = token.getParameterName();
      this.token = token.getToken();
      this.source = source;
    }

    @Override
    public String getHeaderName() {
      return headerName;
    }

    @Override
    public String getParameterName() {
      return parameterName;
    }

    @Override
    public String getToken() {
      return token;
    }

    String getSource() {
      return source;
    }
  }
}
