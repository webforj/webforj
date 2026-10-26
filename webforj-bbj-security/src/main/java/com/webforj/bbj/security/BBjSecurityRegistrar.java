package com.webforj.bbj.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.environment.ObjectTable;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import com.webforj.webstorage.SessionStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.file.Path;
import java.time.Duration;

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

    var properties = new BBjSecurityConfigurationProperties();
    BBjConfigReader.readConfig(properties, config);
    var configuration = new BBjRouteSecurityConfiguration(properties);

    if (!configuration.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    var gateway = new BBjAdminGateway(properties);
    var worker = new BBjAdminWorker(Duration.ofSeconds(properties.getAdminTimeoutSeconds()));
    var locationStore = new BBjBrowserRequestedLocationStore();
    var sessionStore = new BBjFileSessionStore(() -> Path.of(System.getProperty("java.io.tmpdir")),
        App::getApplicationName, SessionStorage::getCurrent);

    var router = Router.getCurrent();
    if (router != null) {
      var service = BBjAuthenticationService.builder().setGateway(gateway).setWorker(worker)
          .setLocationStore(locationStore).setSessionStore(sessionStore).setProperties(properties)
          .setConfiguration(configuration).setNavigator(router::navigate).build();
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
