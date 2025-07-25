package com.webforj.spring.devtools.livereload;

import org.springframework.boot.devtools.restart.RestartScope;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * Spring application listener that detects DevTools restarts and triggers browser reload.
 *
 * <p>
 * This listener monitors Spring context refresh events to distinguish between initial application
 * startup and subsequent DevTools restarts. On restart, it triggers a browser reload through the
 * {@code LiveReloadService} to refresh connected clients.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@RestartScope
public class LiveReloadListener implements ApplicationListener<ApplicationEvent> {

  private static final System.Logger logger = System.getLogger(LiveReloadListener.class.getName());

  private final LiveReloadService reloadService;

  /**
   * Creates a new DevTools reload listener.
   *
   * @param reloadService the service to use for triggering browser reloads
   */
  public LiveReloadListener(LiveReloadService reloadService) {
    this.reloadService = reloadService;
  }

  /**
   * Handles Spring application events to detect when DevTools completes a restart.
   *
   * <p>
   * On ContextRefreshedEvent:
   * </p>
   * <ul>
   * <li>First occurrence: Marks initial startup, no reload triggered</li>
   * <li>Subsequent occurrences: Indicates DevTools restart, triggers browser reload</li>
   * </ul>
   *
   * @param event the Spring application event
   */
  @Override
  public void onApplicationEvent(ApplicationEvent event) {
    if (event instanceof ContextRefreshedEvent) {
      if (LiveReloadState.hasStartedOnce()) {
        logger.log(System.Logger.Level.INFO,
            "DevTools restart detected, triggering browser reload");
        reloadService.triggerReload();
      } else {
        LiveReloadState.markStarted();
        logger.log(System.Logger.Level.INFO, "Initial application start detected");
      }
    }
  }
}
