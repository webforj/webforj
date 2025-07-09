package com.webforj.spring.devtools.livereload;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Spring Boot auto-configuration for webforJ DevTools browser reload functionality.
 *
 * <p>
 * This configuration activates when:
 * </p>
 * <ul>
 * <li>Spring DevTools is on the classpath</li>
 * <li>webforj.devtools.livereload.enabled=true</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Configuration
@ConditionalOnClass(name = "org.springframework.boot.devtools.restart.Restarter")
@ConditionalOnProperty(prefix = "webforj.devtools.livereload", name = "enabled",
    havingValue = "true", matchIfMissing = false)
@EnableConfigurationProperties(LiveReloadProperties.class)
public class LiveReloadSocketConfiguration {

  private static final System.Logger logger =
      System.getLogger(LiveReloadSocketConfiguration.class.getName());

  /**
   * Creates and configures the live reload service.
   *
   * <p>
   * This bean manages the WebSocket server lifecycle:
   * </p>
   * <ul>
   * <li>On first startup: Creates and starts a new WebSocket server</li>
   * <li>On DevTools restart: Reuses existing server to maintain connections</li>
   * </ul>
   *
   * @param properties configuration properties for the reload functionality
   * @return configured DevTools reload service
   */
  @Bean
  public LiveReloadService liveReloadService(LiveReloadProperties properties) {
    LiveReloadService service = new LiveReloadService();

    // Start the WebSocket server if not already started
    LiveReloadServer existingServer = LiveReloadState.getWebSocketServer();
    if (existingServer == null || !existingServer.isOpen()) {
      int port = properties.getWebsocketPort();
      try {
        LiveReloadServer server = new LiveReloadServer(port);
        server.start();
        LiveReloadState.setWebSocketServer(server);
        logger.log(System.Logger.Level.INFO, "Started webforJ livereload server on port " + port);
      } catch (Exception e) {
        logger.log(System.Logger.Level.ERROR,
            "Failed to start webforJ livereload server on port " + port, e);
      }
    } else {
      logger.log(System.Logger.Level.INFO,
          "webforJ livereload server already running on port " + existingServer.getPort() + " with "
              + existingServer.getConnectionCount() + " connections");
    }

    if (LiveReloadState.getWebSocketServer() != null) {
      service.setWebSocketServer(LiveReloadState.getWebSocketServer());
    }

    return service;
  }

  /**
   * Creates the listener that detects DevTools restarts.
   *
   * <p>
   * This listener monitors application lifecycle events to trigger browser reloads when DevTools
   * completes a restart.
   * </p>
   *
   * @param reloadService service used to trigger browser reloads
   * @return configured DevTools reload listener
   */
  @Bean
  public LiveReloadListener liveReloadListener(LiveReloadService reloadService) {
    return new LiveReloadListener(reloadService);
  }

  /**
   * Creates the listener that detects static resource changes.
   *
   * <p>
   * This listener monitors file system changes to CSS, JavaScript, and image files, enabling hot
   * reloading without full page refresh if possible.
   * </p>
   *
   * @return configured resource change listener
   */
  @Bean
  @ConditionalOnProperty(prefix = "webforj.devtools.livereload", name = "static-resources-enabled",
      havingValue = "true", matchIfMissing = true)
  public LiveReloadResourceChangeListener liveReloadResourceChangeListener() {
    return new LiveReloadResourceChangeListener();
  }
}
