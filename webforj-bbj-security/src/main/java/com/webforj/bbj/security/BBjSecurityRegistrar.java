package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;

/**
 * BBjSecurityRegistrar
 */
@AppListenerPriority(1)
public class BBjSecurityRegistrar implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(BBjSecurityRegistrar.class.getName());

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

    var configuration = BBjConfigReader.readConfig(config);

    if (!configuration.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    var context = new BBjRouteSecurityContext(Optional::empty);
    var manager =
        new BBjRouteSecurityManager(configuration, context, new BBjBrowserRequestedLocationStore());

    var router = Router.getCurrent();
    if (router != null) {
      var securityObserver = new RouteSecurityObserver(manager);
      router.getRenderer().addObserver(securityObserver);
      logger.log(Level.INFO, "Route security enabled with {0} evaluators",
          manager.getEvaluators().size());
    } else {
      logger.log(Level.WARNING, "Router not available, security not registered");
    }
  }

}
