package com.webforj.bbj.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;

/**
 * Registers route security with the router when an app runs in BBjServices.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
@AppListenerPriority(1)
public final class BBjRouteSecurityRegistrar implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(BBjRouteSecurityRegistrar.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    Environment environment = Environment.getCurrent();
    if (environment == null) {
      logger.log(Level.WARNING, "Environment not available, route security disabled");
      return;
    }

    BBjRouteSecurityConfiguration configuration =
        BBjRouteSecurityConfiguration.create(environment.getConfig());
    if (!configuration.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    Router router = Router.getCurrent();
    if (router == null) {
      logger.log(Level.WARNING, "Router not available, route security disabled");
      return;
    }

    BBjRouteSecurityManager manager =
        new BBjRouteSecurityManager(configuration, new BBjAuthenticationState(), Clock.systemUTC());
    router.getRenderer().addObserver(new RouteSecurityObserver(manager));
    logger.log(Level.INFO, "BBj route security enabled with {0} evaluators",
        manager.getEvaluators().size());
  }
}
