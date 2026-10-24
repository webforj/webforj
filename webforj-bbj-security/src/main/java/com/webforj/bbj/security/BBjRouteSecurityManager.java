package com.webforj.bbj.security;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteRelation;
import com.webforj.router.Router;
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
 * <p>
 * A denial sends the visitor to the authentication or deny location and recreates the rendered
 * routes. The router removes the rendered routes before it asks for the denied one, so after a
 * denial on a history navigation its record of the rendered routes lists routes that are already
 * destroyed, and a navigation that reuses them renders nothing.
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
  protected void onPostAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    switch (decision.getDenialType()) {
      case AUTHENTICATION_REQUIRED ->
        getConfiguration().getAuthenticationLocation().ifPresent(this::navigateRecreating);
      case ACCESS_DENIED -> getConfiguration().getDenyLocation().ifPresent(location -> {
        if (decision.getReason() != null && !decision.getReason().isEmpty()) {
          location.getQueryParameters().put("reason", decision.getReason());
        }

        navigateRecreating(location);
      });
      default -> {
        // no location to open
      }
    }
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

  private void navigateRecreating(Location location) {
    Router router = Router.getCurrent();
    if (router == null) {
      return;
    }

    NavigationOptions options = new NavigationOptions();
    options.setNavigationType(NavigationOptions.NavigationType.REPLACE);
    findRenderedRoot(router).ifPresent(options::setRecreateFrom);
    router.navigate(location, options);
  }

  private Optional<Class<? extends Component>> findRenderedRoot(Router router) {
    if (router.getRenderer() == null) {
      return Optional.empty();
    }

    return router.getRenderer().getActiveRoutePath().map(RouteRelation::getData);
  }
}
