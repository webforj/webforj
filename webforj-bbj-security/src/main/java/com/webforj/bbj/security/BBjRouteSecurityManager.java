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
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Objects;
import java.util.Optional;

/**
 * {@link AbstractRouteSecurityManager} of a BBj application.
 *
 * <p>
 * Registers the core evaluators. Users who need to authenticate are sent to the authentication
 * location and the location they requested is saved. Users who are denied access are sent to the
 * deny location with the reason.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjRouteSecurityManager extends AbstractRouteSecurityManager {
  private static final Logger logger = System.getLogger(BBjRouteSecurityManager.class.getName());
  private final BBjRouteSecurityConfiguration configuration;
  private final BBjRequestedLocationStore locationStore;
  private final BBjRouteSecurityContext securityContext;

  /**
   * Creates a manager with the core evaluators registered.
   *
   * @param configuration the route security configuration
   * @param securityContext the route security context
   * @param locationStore the store of the location requested before authentication
   *
   * @throws NullPointerException if {@code configuration}, {@code securityContext} or
   *         {@code locationStore} is null
   */
  BBjRouteSecurityManager(BBjRouteSecurityConfiguration configuration,
      BBjRouteSecurityContext securityContext, BBjRequestedLocationStore locationStore) {
    this.configuration = Objects.requireNonNull(configuration, "configuration");
    this.securityContext = Objects.requireNonNull(securityContext, "securityContext");
    this.locationStore = Objects.requireNonNull(locationStore, "locationStore");

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
  public void clearPreAuthenticationLocation() {
    locationStore.clearLocation();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getPreAuthenticationLocation() {
    return locationStore.readLocation();
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * When authentication is required, the requested location is saved before the user is sent on.
   * </p>
   */
  @Override
  public void onAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    logger.log(Level.DEBUG, "Access denied: type={0}, reason={1}", decision.getDenialType(),
        decision.getReason());

    if (decision.isAuthenticationRequired() && context != null && context.getLocation() != null) {
      locationStore.saveLocation(context.getLocation());
    }

    onPostAccessDenied(decision, context);
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * A denial that requires authentication sends the user to the authentication location. A denial
   * of access sends the user to the deny location and adds the reason as the query parameter
   * {@code reason}.
   * </p>
   */
  @Override
  protected void onPostAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    switch (decision.getDenialType()) {
      case AUTHENTICATION_REQUIRED ->
        configuration.getAuthenticationLocation().ifPresent(this::navigateRecreating);
      case ACCESS_DENIED -> configuration.getDenyLocation().ifPresent(location -> {
        if (decision.getReason() != null && !decision.getReason().isEmpty()) {
          location.getQueryParameters().put("reason", decision.getReason());
        }

        navigateRecreating(location);
      });
    }
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
