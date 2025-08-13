package com.webforj.spring.scope;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.annotation.AppListenerPriority;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;

/**
 * Handles cleanup of Spring scopes when the webforJ application is terminated.
 *
 * <p>
 * This listener ensures that Spring beans in the environment scope have their destruction callbacks
 * executed properly before the application is terminated.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@AppListenerPriority(Integer.MAX_VALUE)
public class SpringScopeCleanup implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(SpringScopeCleanup.class.getName());

  @Override
  public void onWillTerminate(App app) {
    logger.log(Level.DEBUG, "Cleaning up Spring environment scope");

    try {
      EnvironmentScope.cleanup();
      logger.log(Level.DEBUG, "Spring environment scope cleaned up successfully");
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during Spring scope cleanup", e);
    }
  }
}
