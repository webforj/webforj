package com.webforj.router.security;

import com.webforj.Environment;
import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

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
 * @since 25.10
 */
public abstract class AbstractRouteSecurityManager implements RouteSecurityManager {
  /**
   * HTTP session attribute key for storing the requested location before authentication.
   */
  public static final String PRE_AUTH_LOCATION_KEY = "webforj-requested-location";
  private static final Logger logger =
      System.getLogger(AbstractRouteSecurityManager.class.getName());

  private final List<PrioritizedEvaluator> evaluators = new ArrayList<>();

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerEvaluator(RouteSecurityEvaluator evaluator, int priority) {

    synchronized (evaluators) {
      evaluators.add(new PrioritizedEvaluator(evaluator, priority));
      // Sort by priority (lower values first)
      evaluators.sort(Comparator.comparingInt(e -> e.priority));
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

    // Store the requested location for post-login redirect
    Router router = Router.getCurrent();
    if (router != null && context != null && context.getLocation() != null) {
      // Store in HTTP session instead of browser sessionStorage
      Environment.ifPresent(env -> {
        env.getSessionAccessor().ifPresent(accessor -> {
          accessor.access(session -> {
            session.setAttribute(PRE_AUTH_LOCATION_KEY, context.getLocation().getFullURI());
            logger.log(Level.DEBUG,
                "Stored requested location in HTTP session: " + context.getLocation().getFullURI());
          });
        });
      });
    }

    onPostAccessDenied(decision, context);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getPreAuthenticationLocation() {
    AtomicReference<String> uriRef = new AtomicReference<>();
    Environment.ifPresent(env -> {
      env.getSessionAccessor().ifPresent(accessor -> {
        accessor.access(session -> {
          Object value = session.getAttribute(PRE_AUTH_LOCATION_KEY);
          if (value instanceof String) {
            uriRef.set((String) value);
          }
        });
      });
    });

    String uri = uriRef.get();
    if (uri != null && !uri.isEmpty()) {
      return Optional.of(new Location(uri));
    }

    return Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearPreAuthenticationLocation() {
    Environment.ifPresent(env -> {
      env.getSessionAccessor().ifPresent(accessor -> {
        accessor.access(session -> {
          session.removeAttribute(PRE_AUTH_LOCATION_KEY);
          logger.log(Level.DEBUG, "Cleared pre-authentication location from HTTP session");
        });
      });
    });
  }

  /**
   * Handles post-access-denied actions such as redirects.
   *
   * <p>
   * This method is called after an access denial decision has been made. It performs actions based
   * on the type of denial, such as redirecting to authentication or denial pages as configured.
   * </p>
   *
   * @param decision the access denial decision
   * @param context the current navigation context
   */
  protected void onPostAccessDenied(RouteAccessDecision decision, NavigationContext context) {
    switch (decision.getDenialType()) {
      case AUTHENTICATION_REQUIRED:
        getConfiguration().getAuthenticationLocation().ifPresent(location -> {
          navigateTo(location, true);
        });
        break;

      case ACCESS_DENIED:
        getConfiguration().getDenyLocation().ifPresent(location -> {
          // Add the reason as a query parameter if provided
          if (decision.getReason() != null && !decision.getReason().isEmpty()) {
            location.getQueryParameters().put("reason", decision.getReason());
          }
          navigateTo(location, false);
        });
        break;

      default:
        break;
    }
  }

  private void navigateTo(Location location, boolean shouldUpdateHistory) {
    Router router = Router.getCurrent();
    if (router != null) {
      NavigationOptions options = new NavigationOptions();
      options.setNavigationType(NavigationOptions.NavigationType.REPLACE);
      options.setUpdateHistory(shouldUpdateHistory);
      router.navigate(location, options);
    }
  }

  /**
   * Wrapper to hold evaluator with its priority.
   */
  private static class PrioritizedEvaluator {
    final RouteSecurityEvaluator evaluator;
    final int priority;

    PrioritizedEvaluator(RouteSecurityEvaluator evaluator, int priority) {
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
