package com.webforj.spring.scope;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.spring.scope.processor.EnvironmentScopeProcessor;
import com.webforj.spring.scope.processor.RouteScopeProcessor;
import com.webforj.spring.scope.processor.SessionScopeProcessor;
import jakarta.servlet.http.HttpSessionEvent;
import jakarta.servlet.http.HttpSessionListener;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;

/**
 * Handles cleanup of Spring scopes for both application and session lifecycles.
 *
 * <p>
 * This listener ensures that Spring beans in all scopes have their destruction callbacks executed
 * properly when the application is terminated or sessions are destroyed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@AppListenerPriority(Integer.MAX_VALUE)
public class SpringScopeCleanup implements AppLifecycleListener, HttpSessionListener {
  private static final Logger logger = System.getLogger(SpringScopeCleanup.class.getName());

  @Override
  public void onWillTerminate(App app) {
    logger.log(Level.DEBUG, "Cleaning up Spring scopes");

    try {
      EnvironmentScopeProcessor.cleanup();
      logger.log(Level.DEBUG, "Spring environment scope cleaned up successfully");

      RouteScopeProcessor.cleanup();
      logger.log(Level.DEBUG, "Spring route scopes cleaned up successfully");
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during Spring scope cleanup", e);
    }
  }

  @Override
  public void sessionDestroyed(HttpSessionEvent se) {
    logger.log(Level.DEBUG, "HTTP session destroyed: {0}, cleaning up session scope",
        se.getSession().getId());
    try {
      SessionScopeProcessor.cleanup();
      logger.log(Level.DEBUG, "Session scope cleaned up successfully for session: {0}",
          se.getSession().getId());
    } catch (Exception e) {
      logger.log(Level.ERROR,
          "Could not cleanup session scope for session: {0} - session context not available",
          se.getSession().getId());
    }
  }
}
