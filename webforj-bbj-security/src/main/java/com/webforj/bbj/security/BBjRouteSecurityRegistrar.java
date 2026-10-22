package com.webforj.bbj.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.environment.ObjectTable;
import com.webforj.environment.namespace.GlobalNamespace;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import com.webforj.webstorage.SessionStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.function.Function;

/**
 * Registers route security with the router when an app runs in BBjServices.
 *
 * <p>
 * Once route security is enabled, a failure while registering it or an invalid connection setting
 * of the Admin API stops the app start with a {@link BBjSecurityError}, so the app never runs
 * without route security.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
@AppListenerPriority(1)
public final class BBjRouteSecurityRegistrar implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(BBjRouteSecurityRegistrar.class.getName());

  /**
   * {@inheritDoc}
   *
   * @throws BBjSecurityError when route security is enabled and cannot be registered
   */
  @Override
  public void onWillRun(App app) {
    try {
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

      BBjAdminConfiguration adminConfiguration =
          BBjAdminConfiguration.create(environment.getConfig());
      logger.log(Level.INFO, "BBj admin connection {0}", adminConfiguration);

      Router router = Router.getCurrent();
      if (router == null) {
        logger.log(Level.WARNING, "Router not available, route security disabled");
        return;
      }

      Clock clock = Clock.systemUTC();
      BBjAuthenticationState state = new BBjAuthenticationState();
      BBjRouteSecurityManager manager = new BBjRouteSecurityManager(configuration, state, clock,
          new BBjBrowserRequestedLocationStore(BBjRouteSecurityRegistrar::findSessionStorage));
      BBjRouteSecurityEvaluatorLoader.loadEvaluators(Thread.currentThread().getContextClassLoader())
          .forEach(evaluator -> manager.registerEvaluator(evaluator, evaluator.getPriority()));

      BBjAdminTaskRunner runner = new BBjAdminTaskRunner(adminConfiguration.getTimeout(),
          BBjRouteSecurityRegistrar::createDispatcher);
      BBjAuthenticationService service = new BBjAuthenticationService(
          new BBjFactoryAdminGateway(adminConfiguration, clock), runner,
          new BBjNamespaceTokenStore(GlobalNamespace::new, App::getApplicationName,
              SessionStorage::getCurrent, clock),
          state, manager::consumePreAuthenticationLocation, location -> router.navigate(location));

      ObjectTable.put(BBjRouteSecurityContext.class.getName(), manager.getSecurityContext());
      ObjectTable.put(BBjAdminTaskRunner.class.getName(), runner);
      ObjectTable.put(BBjAuthenticationService.class.getName(), service);
      router.getRenderer().addObserver(new RouteSecurityObserver(manager));
      logger.log(Level.INFO, "BBj route security enabled with {0} evaluators",
          manager.getEvaluators().size());
    } catch (RuntimeException e) {
      logger.log(Level.ERROR, "Route security could not be registered, the app does not start", e);
      throw new BBjSecurityError("Route security could not be registered", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillTerminate(App app) {
    try {
      String key = BBjAdminTaskRunner.class.getName();
      if (ObjectTable.contains(key)) {
        ((BBjAdminTaskRunner) ObjectTable.get(key)).destroy();
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The Admin API task runner could not be stopped, {0}",
          e.getClass().getName());
    }
  }

  private static Function<Runnable, PendingResult<Void>> createDispatcher() {
    Environment environment = Environment.getCurrent();

    return task -> Environment.runLater(environment, task);
  }

  private static SessionStorage findSessionStorage() {
    return Environment.getCurrent() == null ? null : SessionStorage.getCurrent();
  }
}
