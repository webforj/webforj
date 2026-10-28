package com.webforj.spring.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Page;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import com.webforj.spring.ContextHolder;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;
import org.springframework.context.ApplicationContext;
import org.springframework.security.web.csrf.CsrfToken;

/**
 * Registers route security components during application startup.
 *
 * <p>
 * Integrates Spring Security with webforJ routing by registering the security manager and
 * evaluators with the router's renderer, and publishes the session CSRF token to the page.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@AppListenerPriority(1)
public class RouteSecurityObserverRegistrar implements AppLifecycleListener {
  private static final Logger logger =
      System.getLogger(RouteSecurityObserverRegistrar.class.getName());
  private static final String CSRF_TOKEN_META = "_csrf";
  private static final String CSRF_PARAMETER_META = "_csrf_parameter";

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    // Get Spring context via ContextHolder
    ApplicationContext ctx = ContextHolder.getContext();
    if (ctx == null) {
      logger.log(Level.WARNING, "Spring context not available, route security disabled");
      return;
    }

    // Check if security configuration exists and is enabled
    SpringRouteSecurityConfiguration securityConfig;
    try {
      securityConfig = ctx.getBean(SpringRouteSecurityConfiguration.class);
    } catch (Exception e) {
      logger.log(Level.DEBUG, "SpringRouteSecurityConfiguration not found, security disabled", e);
      return;
    }

    addCsrfMeta();

    if (!securityConfig.isEnabled()) {
      logger.log(Level.INFO, "Route security disabled by configuration");
      return;
    }

    // Get SpringRouteSecurityManager from Spring context
    SpringRouteSecurityManager securityManager;
    try {
      securityManager = ctx.getBean(SpringRouteSecurityManager.class);
    } catch (Exception e) {
      logger.log(Level.ERROR, "SpringRouteSecurityManager not found in context", e);
      return;
    }

    // Create security observer
    RouteSecurityObserver securityObserver = new RouteSecurityObserver(securityManager);
    Router router = Router.getCurrent();
    if (router != null) {
      router.getRenderer().addObserver(securityObserver);
      logger.log(Level.INFO, "Route security enabled with {0} evaluators",
          securityManager.getEvaluators().size());
    } else {
      logger.log(Level.WARNING, "Router not available, security not registered");
    }
  }

  private void addCsrfMeta() {
    Optional<CsrfToken> token = WebforjCsrfTokenFilter.findToken();
    if (token.isPresent()) {
      Page page = Page.getCurrent();
      page.setMeta(CSRF_TOKEN_META, token.get().getToken());
      page.setMeta(CSRF_PARAMETER_META, token.get().getParameterName());
    }
  }
}
