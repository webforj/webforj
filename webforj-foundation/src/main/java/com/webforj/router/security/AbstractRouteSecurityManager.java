package com.webforj.router.security;

import com.webforj.router.NavigationContext;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.webstorage.SessionStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Abstract implementation of {@link RouteSecurityManager}.
 *
 * <p>
 * Provides common functionality for managing evaluators and handling access decisions. Concrete
 * implementations should provide {@link RouteSecurityContext} and
 * {@link RouteSecurityConfiguration}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public abstract class AbstractRouteSecurityManager implements RouteSecurityManager {
  private static final Logger logger =
      System.getLogger(AbstractRouteSecurityManager.class.getName());
  private final List<PrioritizedEvaluator> evaluators = new ArrayList<>();

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerEvaluator(RouteSecurityEvaluator evaluator, double priority) {
    if (priority <= 0.0) {
      throw new IllegalArgumentException(
          "Evaluator priority must be greater than 0. Got: " + priority);
    }

    synchronized (evaluators) {
      evaluators.add(new PrioritizedEvaluator(evaluator, priority));
      // Sort by priority (lower values first)
      evaluators.sort(Comparator.comparingDouble(e -> e.priority));
    }

    logger.log(Level.DEBUG, "Registered evaluator: {0} with priority {1}",
        evaluator.getClass().getName(), priority);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void unregisterEvaluator(RouteSecurityEvaluator evaluator) {
    synchronized (evaluators) {
      evaluators.removeIf(pe -> pe.evaluator.equals(evaluator));
    }

    logger.log(Level.DEBUG, "Unregistered evaluator: {0}", evaluator.getClass().getName());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<RouteSecurityEvaluator> getEvaluators() {
    synchronized (evaluators) {
      List<RouteSecurityEvaluator> result = new ArrayList<>();
      for (PrioritizedEvaluator pe : evaluators) {
        result.add(pe.evaluator);
      }

      return Collections.unmodifiableList(result);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context) {
    // Check if security is enabled
    if (!getConfiguration().isEnabled()) {
      return RouteAccessDecision.grant();
    }

    RouteSecurityContext securityContext = getSecurityContext();

    // Create and start the chain
    List<PrioritizedEvaluator> evaluatorsCopy;
    synchronized (evaluators) {
      evaluatorsCopy = new ArrayList<>(evaluators);
    }

    SecurityEvaluatorChain chain =
        new SecurityEvaluatorChainImpl(evaluatorsCopy, 0, getConfiguration());

    return chain.evaluate(routeClass, context, securityContext);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    logger.log(Level.DEBUG, "Access denied: type={0}, reason={1}", decision.getDenialType(),
        decision.getReason());

    switch (decision.getDenialType()) {
      case AUTHENTICATION_REQUIRED:
        onAuthenticationRequired(decision, context);
        break;

      case INSUFFICIENT_PERMISSIONS:
        onInsufficientPermissions(decision, context);
        break;

      case CUSTOM_DENIAL:
        onCustomDenial(decision, context);
        break;

      default:
        logger.log(Level.WARNING, "Unknown denial type: {0}", decision.getDenialType());
    }
  }

  /**
   * Called when authentication is required.
   *
   * <p>
   * Default implementation redirects to the authentication location if configured. Subclasses can
   * override to customize behavior, store the requested location for post-login redirect, show
   * login modals, or handle authentication differently.
   * </p>
   *
   * @param decision the access decision with denial details
   * @param context the navigation context
   */
  protected void onAuthenticationRequired(RouteAccessDecision decision, NavigationContext context) {
    // Store the requested location for post-login redirect
    Router router = Router.getCurrent();
    if (router != null && context != null && context.getLocation() != null) {
      try {
        SessionStorage.getCurrent().setItem("webforj-requested-location",
            context.getLocation().getFullURI());
      } catch (Exception e) {
        // Log but don't fail if storage is unavailable
        logger.log(Level.DEBUG, "Could not store requested location", e);
      }
    }

    getConfiguration().getAuthenticationLocation().ifPresent(location -> {
      if (router != null) {
        router.navigate(location);
      }
    });
  }

  /**
   * Called when user has insufficient permissions.
   *
   * <p>
   * Default implementation redirects to the insufficient permissions location if configured.
   * Subclasses can override to show error messages, log security violations, notify administrators,
   * or handle authorization failures differently.
   * </p>
   *
   * @param decision the access decision with denial details
   * @param context the navigation context
   */
  protected void onInsufficientPermissions(RouteAccessDecision decision,
      NavigationContext context) {
    getConfiguration().getInsufficientPermissionsLocation().ifPresent(location -> {
      Router router = Router.getCurrent();
      if (router != null) {
        router.navigate(location);
      }
    });
  }

  /**
   * Called when custom denial occurs.
   *
   * <p>
   * Default implementation redirects to the custom denial location if configured (which defaults to
   * the insufficient permissions location). Subclasses can override to provide different handling
   * such as showing error dialogs or custom navigation.
   * </p>
   *
   * @param decision the access decision with custom denial
   * @param context the navigation context
   */
  protected void onCustomDenial(RouteAccessDecision decision, NavigationContext context) {
    getConfiguration().getCustomDenialLocation().ifPresent(location -> {
      Router router = Router.getCurrent();
      if (router != null) {
        router.navigate(location);
      }
    });
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getPreAuthenticationLocation() {
    try {
      String uri = SessionStorage.getCurrent().getItem("webforj-requested-location");
      if (uri != null && !uri.isEmpty()) {
        return Optional.of(new Location(uri));
      }
    } catch (Exception e) {
      logger.log(Level.DEBUG, "Could not retrieve pre-authentication location", e);
    }
    return Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearPreAuthenticationLocation() {
    try {
      SessionStorage.getCurrent().removeItem("webforj-requested-location");
    } catch (Exception e) {
      logger.log(Level.DEBUG, "Could not clear pre-authentication location", e);
    }
  }

  /**
   * Wrapper to hold evaluator with its priority.
   */
  private static class PrioritizedEvaluator {
    final RouteSecurityEvaluator evaluator;
    final double priority;

    PrioritizedEvaluator(RouteSecurityEvaluator evaluator, double priority) {
      this.evaluator = evaluator;
      this.priority = priority;
    }
  }

  /**
   * Implementation of the evaluator chain.
   */
  private static class SecurityEvaluatorChainImpl implements SecurityEvaluatorChain {
    private final List<PrioritizedEvaluator> evaluators;
    private final int currentIndex;
    private final RouteSecurityConfiguration configuration;

    SecurityEvaluatorChainImpl(List<PrioritizedEvaluator> evaluators, int index,
        RouteSecurityConfiguration configuration) {
      this.evaluators = evaluators;
      this.currentIndex = index;
      this.configuration = configuration;
    }

    @Override
    public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
        RouteSecurityContext securityContext) {

      // Find next evaluator that supports this route
      for (int i = currentIndex; i < evaluators.size(); i++) {
        PrioritizedEvaluator pe = evaluators.get(i);
        RouteSecurityEvaluator evaluator = pe.evaluator;

        if (evaluator.supports(routeClass)) {
          // Create chain for next evaluator
          SecurityEvaluatorChain nextChain =
              new SecurityEvaluatorChainImpl(evaluators, i + 1, configuration);

          try {
            // Let current evaluator process
            RouteAccessDecision decision =
                evaluator.evaluate(routeClass, context, securityContext, nextChain);

            logger.log(Level.DEBUG, "Evaluator {0} returned decision: granted={1}",
                evaluator.getClass().getName(), decision.isGranted());

            return decision;
          } catch (Exception e) {
            logger.log(Level.ERROR, "Error in evaluator {0}", evaluator.getClass().getName(), e);
            // On error, deny access for safety
            return RouteAccessDecision.deny("Security evaluation error");
          }
        }
      }

      // No more evaluators - apply default behavior
      if (configuration.isSecureByDefault() && !securityContext.isAuthenticated()) {
        logger.log(Level.DEBUG, "No evaluator handled route, applying secure-by-default");
        return RouteAccessDecision.denyAuthentication();
      }

      return RouteAccessDecision.grant();
    }
  }
}
