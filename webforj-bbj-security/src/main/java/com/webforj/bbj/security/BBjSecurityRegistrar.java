package com.webforj.bbj.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.bbj.security.store.BBjRequestedLocationStore;
import com.webforj.bbj.security.store.BBjSessionStore;
import com.webforj.environment.ObjectTable;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;

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
public final class BBjSecurityRegistrar implements AppLifecycleListener {
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

    var properties = new BBjSecurityConfigurationProperties(config);
    var configuration = new BBjRouteSecurityConfiguration(properties);

    if (!configuration.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    var gateway = new BBjAdminGateway(properties);
    var locationStore = BBjRequestedLocationStore.create(environment);
    var sessionStore = BBjSessionStore.create(environment);

    var router = Router.getCurrent();
    if (router != null) {
      var service = BBjAuthenticationService.builder().setGateway(gateway)
          .setLocationStore(locationStore).setSessionStore(sessionStore).setProperties(properties)
          .setConfiguration(configuration).setNavigator(router::navigate).build();
      var observer = new BBjSessionObserver(service);
      router.getRenderer().addObserver(observer);
      ObjectTable.put(BBjSecurity.SERVICE_KEY, service);
      service.restoreSession();
      var context = new BBjRouteSecurityContext(service::currentUser);
      var manager = new BBjRouteSecurityManager(configuration, context, locationStore);
      var securityObserver = new RouteSecurityObserver(manager);
      router.getRenderer().addObserver(securityObserver);
      logger.log(Level.INFO, "Route security enabled with {0} evaluators",
          manager.getEvaluators().size());
    } else {
      logger.log(Level.WARNING, "Router not available, security not registered");
    }
  }

}
