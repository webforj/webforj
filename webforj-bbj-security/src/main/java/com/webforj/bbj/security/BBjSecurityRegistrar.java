package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Duration;
import java.util.Optional;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.environment.ObjectTable;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;

/**
 * Registers route security for a BBj application when it starts.
 *
 * <p>
 * When security is enabled in the configuration, the route security observer is added to the
 * router. Nothing is registered when the environment, the configuration or the router is not
 * available.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
@AppListenerPriority(1)
public class BBjSecurityRegistrar implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(BBjSecurityRegistrar.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    var environment = Environment.getCurrent();
    if (environment == null) {
      logger.log(Level.WARNING, "Environment not available, route security disabled");
      return;
    }

    var config = environment.getConfig();
    if (config == null) {
      logger.log(Level.WARNING, "Config not available, route security disabled");
      return;
    }

    var properties = new BBjSecurityConfigurationProperties();
    BBjConfigReader.readConfig(properties, config);
    var configuration = new BBjRouteSecurityConfiguration(properties);

    if (!configuration.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    var gateway = new BBjAdminGateway(properties);
    var worker = new BBjAdminWorker(Duration.ofSeconds(properties.getAdminTimeoutSeconds()));
    var store = new BBjBrowserRequestedLocationStore();

    var router = Router.getCurrent();
    if (router != null) {
      var service =
          new BBjAuthenticationService(gateway, worker, store, properties, router::navigate);
      ObjectTable.put(BBjSecurity.SERVICE_KEY, service);
      var context = new BBjRouteSecurityContext(service::currentUser);
      var manager = new BBjRouteSecurityManager(configuration, context,
          new BBjBrowserRequestedLocationStore());
      var securityObserver = new RouteSecurityObserver(manager);
      router.getRenderer().addObserver(securityObserver);
      logger.log(Level.INFO, "Route security enabled with {0} evaluators",
          manager.getEvaluators().size());
    } else {
      logger.log(Level.WARNING, "Router not available, security not registered");
    }
  }

}
