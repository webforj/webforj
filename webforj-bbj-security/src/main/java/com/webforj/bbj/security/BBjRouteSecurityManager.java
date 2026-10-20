package com.webforj.bbj.security;

import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteSecurityConfiguration;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import java.time.Clock;

/**
 * Route security manager of an app running in BBjServices.
 *
 * <p>
 * Registers the built-in evaluators. The security context reads the authentication state of the
 * app.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityManager extends AbstractRouteSecurityManager {
  private final BBjRouteSecurityConfiguration configuration;
  private final RouteSecurityContext securityContext;

  BBjRouteSecurityManager(BBjRouteSecurityConfiguration configuration,
      BBjAuthenticationState authenticationState, Clock clock) {
    this.configuration = configuration;
    this.securityContext = new BBjRouteSecurityContext(authenticationState, clock);

    registerEvaluator(new DenyAllEvaluator(), 1);
    registerEvaluator(new AnonymousAccessEvaluator(), 2);
    registerEvaluator(new AuthenticationRequiredEvaluator(), 3);
    registerEvaluator(new PermitAllEvaluator(), 4);
    registerEvaluator(new RolesAllowedEvaluator(), 5);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteSecurityContext getSecurityContext() {
    return securityContext;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteSecurityConfiguration getConfiguration() {
    return configuration;
  }
}
