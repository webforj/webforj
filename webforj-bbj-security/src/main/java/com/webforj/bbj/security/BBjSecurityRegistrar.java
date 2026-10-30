package com.webforj.bbj.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.bbj.security.store.BBjSecurityStore;
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
 * <p>
 * When the connection settings of the BBj Admin API are refused, the error is logged and route
 * security is registered without the sign in service, so every sign in is unavailable.
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

    var problem = properties.findAdminConnectionProblem();
    if (problem.isPresent()) {
      logger.log(Level.ERROR, "Sign in is unavailable because {0}", problem.get());
    }

    var store = BBjSecurityStore.create(environment);
    var router = Router.getCurrent();
    if (router != null) {
      if (problem.isEmpty()) {
        addService(router, properties, configuration, store);
      }

      var context = new BBjRouteSecurityContext();
      var manager = new BBjRouteSecurityManager(configuration, context, store);
      var securityObserver = new RouteSecurityObserver(manager);
      router.getRenderer().addObserver(securityObserver);
      logger.log(Level.INFO, "Route security enabled with {0} evaluators",
          manager.getEvaluators().size());
    } else {
      logger.log(Level.WARNING, "Router not available, security not registered");
    }
  }

  private static void addService(Router router, BBjSecurityConfigurationProperties properties,
      BBjRouteSecurityConfiguration configuration, BBjSecurityStore store) {
    var gateway = new BBjAdminGateway(properties);
    logger.log(Level.INFO, "BBj Admin API at {0}:{1} with ssl {2} and truststore {3}",
        properties.getAdminHost(), String.valueOf(properties.getAdminPort()),
        properties.getAdminSsl(), properties.getAdminTruststore() == null ? "none" : "set");

    var service = BBjAuthenticationService.builder().setGateway(gateway).setStore(store)
        .setProperties(properties).setConfiguration(configuration).setNavigator(router::navigate)
        .build();
    var observer = new BBjSessionObserver(service);
    router.getRenderer().addObserver(observer);
    ObjectTable.put(BBjSecurity.SERVICE_KEY, service);
    service.restoreSession();
  }
}
