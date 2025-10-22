package com.webforj.spring.security;

import static com.webforj.router.security.AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Optional;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;

/**
 * Authentication success handler that integrates with webforJ's route security.
 *
 * <p>
 * This handler checks for a pre-authentication location stored by webforJ's route security manager
 * and redirects there after successful authentication. If no location is stored, it falls back to
 * Spring Security's default behavior.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class WebforjAuthenticationSuccessHandler
    extends SavedRequestAwareAuthenticationSuccessHandler {
  /**
   * {@inheritDoc}
   */
  @Override
  public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
      Authentication authentication) throws ServletException, IOException {

    // Check if webforJ stored a location in HTTP session
    String storedLocation = Optional.ofNullable(request.getSession())
        .map(session -> session.getAttribute(PRE_AUTH_LOCATION_KEY))
        .filter(String.class::isInstance).map(String.class::cast).orElse(null);

    if (storedLocation != null) {
      // Clear the stored location
      request.getSession().removeAttribute(PRE_AUTH_LOCATION_KEY);

      // Redirect to the stored location
      getRedirectStrategy().sendRedirect(request, response, storedLocation);
      return;
    }

    // Fall back to Spring Security's default behavior
    super.onAuthenticationSuccess(request, response, authentication);
  }
}
