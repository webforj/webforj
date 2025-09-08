package com.webforj.router.security;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.observer.RouteRendererObserver;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.function.Consumer;

/**
 * Observer that integrates security checks into the route rendering lifecycle.
 *
 * <p>
 * This observer delegates security evaluation to a {@link RouteSecurityManager}. It intercepts
 * route creation and ensures security requirements are met before allowing routes to render.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public class RouteSecurityObserver implements RouteRendererObserver {
  private static final Logger logger = System.getLogger(RouteSecurityObserver.class.getName());
  private final RouteSecurityManager securityManager;

  /**
   * Creates a new RouteSecurityObserver with the given security manager.
   *
   * @param securityManager the security manager to use for evaluation
   */
  public RouteSecurityObserver(RouteSecurityManager securityManager) {
    this.securityManager = securityManager;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> continueCallback) {

    // Only evaluate access before creating the route
    if (event != LifecycleEvent.BEFORE_CREATE) {
      continueCallback.accept(true);
      return;
    }

    Class<?> routeClass = component.getClass();
    logger.log(Level.DEBUG, "Security check for route: {0}", routeClass.getName());

    try {
      RouteAccessDecision decision = securityManager.evaluate(routeClass, context);

      if (decision.isGranted()) {
        logger.log(Level.DEBUG, "Access granted to route: {0}", routeClass.getName());
        continueCallback.accept(true);
      } else {
        logger.log(Level.DEBUG, "Access denied to route: {0}, reason: {1}", routeClass.getName(),
            decision.getReason());
        continueCallback.accept(false);
        securityManager.onAccessDenied(decision, context);
      }
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during security evaluation for route: {0}",
          routeClass.getName(), e);
      // On error, deny access for safety
      continueCallback.accept(false);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 1;
  }

  /**
   * Gets the security manager used by this observer.
   *
   * @return the security manager
   */
  public RouteSecurityManager getSecurityManager() {
    return securityManager;
  }
}
