package com.webforj.bbj.security;

import com.webforj.router.NavigationContext;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityConfiguration;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.Optional;

/**
 * Route security manager of an app running in BBjServices.
 *
 * <p>
 * Registers the built-in evaluators. The security context reads the authentication state of the
 * app.
 * </p>
 *
 * <p>
 * The location a visitor asked for is kept in the {@link BBjRequestedLocationStore} and only when
 * an anonymous visitor is sent to authenticate. A denial for a signed in visitor leaves the kept
 * location as it is.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityManager extends AbstractRouteSecurityManager {
  private static final Logger logger = System.getLogger(BBjRouteSecurityManager.class.getName());

  private final BBjRouteSecurityConfiguration configuration;
  private final BBjRouteSecurityContext securityContext;
  private final BBjRequestedLocationStore requestedLocationStore;

  BBjRouteSecurityManager(BBjRouteSecurityConfiguration configuration,
      BBjAuthenticationState authenticationState, Clock clock,
      BBjRequestedLocationStore requestedLocationStore) {
    this.configuration = configuration;
    this.requestedLocationStore = requestedLocationStore;
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
  public BBjRouteSecurityContext getSecurityContext() {
    return securityContext;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteSecurityConfiguration getConfiguration() {
    return configuration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    logger.log(Level.DEBUG, "Access denied: type={0}, reason={1}", decision.getDenialType(),
        decision.getReason());

    if (decision.isAuthenticationRequired() && context != null && context.getLocation() != null) {
      requestedLocationStore.saveLocation(context.getLocation());
    }

    onPostAccessDenied(decision, context);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getPreAuthenticationLocation() {
    return requestedLocationStore.readLocation();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearPreAuthenticationLocation() {
    requestedLocationStore.clearLocation();
  }
}
