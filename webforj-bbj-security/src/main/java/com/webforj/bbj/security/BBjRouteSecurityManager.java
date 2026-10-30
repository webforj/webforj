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
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * {@link AbstractRouteSecurityManager} of a BBj application.
 *
 * <p>
 * Registers the core evaluators and the {@link BBjPermissionEvaluator}, then each
 * {@link BBjRouteSecurityEvaluator} listed in a provider file at its own priority. The location a
 * user requested is saved on every denial. Users who need to authenticate are sent to the
 * authentication location. Users who are denied access are sent to the deny location with the
 * reason.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjRouteSecurityManager extends AbstractRouteSecurityManager {
  private static final Logger logger = System.getLogger(BBjRouteSecurityManager.class.getName());
  private final BBjRouteSecurityConfiguration configuration;
  private final BBjRouteSecurityContext securityContext;

  /**
   * Creates a manager with the core evaluators, the {@link BBjPermissionEvaluator} and the listed
   * {@link BBjRouteSecurityEvaluator} instances registered.
   *
   * @param configuration the route security configuration
   * @param securityContext the route security context
   *
   * @throws NullPointerException if {@code configuration} or {@code securityContext} is null
   * @throws ServiceConfigurationError if a listed evaluator cannot be loaded
   */
  BBjRouteSecurityManager(BBjRouteSecurityConfiguration configuration,
      BBjRouteSecurityContext securityContext) {
    this.configuration = Objects.requireNonNull(configuration, "configuration");
    this.securityContext = Objects.requireNonNull(securityContext, "securityContext");

    registerEvaluator(new DenyAllEvaluator(), 1);
    registerEvaluator(new AnonymousAccessEvaluator(), 2);
    registerEvaluator(new AuthenticationRequiredEvaluator(), 3);
    registerEvaluator(new PermitAllEvaluator(), 4);
    registerEvaluator(new RolesAllowedEvaluator(), 5);
    registerEvaluator(new BBjPermissionEvaluator(), 6);
    loadEvaluators();
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

  private void loadEvaluators() {
    ServiceLoader<BBjRouteSecurityEvaluator> loader = ServiceLoader
        .load(BBjRouteSecurityEvaluator.class, BBjRouteSecurityManager.class.getClassLoader());
    loader.forEach(evaluator -> {
      String name = evaluator.getClass().getName();
      int priority = evaluator.getPriority();
      if (priority < 10) {
        logger.log(Level.WARNING,
            "Custom evaluator {0} uses priority {1} in core range (0 to 9). "
                + "Consider using priority >= 10 for custom evaluators",
            name, String.valueOf(priority));
      }

      registerEvaluator(evaluator, priority);
      logger.log(Level.INFO, "Registered custom evaluator: {0} with priority {1}", name,
          String.valueOf(priority));
    });
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
