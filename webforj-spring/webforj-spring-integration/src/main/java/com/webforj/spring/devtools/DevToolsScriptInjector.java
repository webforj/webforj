package com.webforj.spring.devtools;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.spring.ContextHolder;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.springframework.context.ApplicationContext;

/**
 * webforJ application lifecycle listener that injects the DevTools reload client script.
 *
 * <p>
 * This injector runs with maximum priority to ensure the reload script is added to the page before
 * any application content. It checks for DevTools presence and configuration, then injects
 * JavaScript that establishes a WebSocket connection to the DevTools server for automatic browser
 * reload on application restarts.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@AppListenerPriority(0)
public class DevToolsScriptInjector implements AppLifecycleListener {

  private static final System.Logger logger =
      System.getLogger(DevToolsScriptInjector.class.getName());
  private static final String SCRIPT_RESOURCE = "/META-INF/resources/devtools-reload-client.js";
  private String cachedScript;

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    logger.log(System.Logger.Level.DEBUG,
        "DevToolsScriptInjector.onWillRun called for app: " + app.getId());

    // Check if DevTools reload is enabled
    if (!isDevToolsReloadEnabled()) {
      logger.log(System.Logger.Level.DEBUG, "webforJ DevTools reload is not enabled");
      return;
    }

    // Check if Spring DevTools is present
    if (!isSpringDevToolsPresent()) {
      logger.log(System.Logger.Level.DEBUG, "Spring DevTools is not present");
      return;
    }

    logger.log(System.Logger.Level.DEBUG, "DevTools conditions met, will inject script on app run");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
    logger.log(System.Logger.Level.DEBUG,
        "DevToolsScriptInjector.onDidRun called for app: " + app.getId());

    if (!isDevToolsReloadEnabled() || !isSpringDevToolsPresent()) {
      return;
    }

    try {
      injectReloadScript(app);
    } catch (Exception e) {
      logger.log(System.Logger.Level.WARNING, "Failed to inject webforJ DevTools reload script", e);
    }
  }

  /**
   * Checks if Spring Boot DevTools is available on the classpath.
   *
   * @return true if DevTools is present, false otherwise
   */
  private boolean isSpringDevToolsPresent() {
    try {
      Class.forName("org.springframework.boot.devtools.restart.Restarter");
      return true;
    } catch (ClassNotFoundException e) {
      return false;
    }
  }

  /**
   * Retrieves a configuration property value from multiple sources.
   *
   * <p>
   * Search order:
   * </p>
   * <ol>
   * <li>Spring Environment (if available)</li>
   * <li>System properties</li>
   * <li>Environment variables (with dot-to-underscore conversion)</li>
   * </ol>
   *
   * @param key the property key (e.g., "webforj.devtools.reload.enabled")
   * @param defaultValue value to return if property is not found
   * @return the property value or default
   */
  private String getProperty(String key, String defaultValue) {
    try {
      ApplicationContext ctx = ContextHolder.getContext();
      if (ctx != null) {
        String value = ctx.getEnvironment().getProperty(key);
        if (value != null) {
          return value;
        }
      }
    } catch (Exception e) {
      logger.log(System.Logger.Level.WARNING, "Could not access Spring Environment", e);
    }

    // Try system properties
    String value = System.getProperty(key);
    if (value != null) {
      return value;
    }

    // Try environment variables (convert dots to underscores and uppercase)
    String envKey = key.replace(".", "_").toUpperCase();
    value = System.getenv(envKey);
    if (value != null) {
      return value;
    }

    return defaultValue;
  }

  /**
   * Checks if DevTools reload functionality is enabled in configuration.
   *
   * @return true if webforj.devtools.reload.enabled=true, false otherwise
   */
  private boolean isDevToolsReloadEnabled() {
    return Boolean.parseBoolean(getProperty("webforj.devtools.reload.enabled", "false"));
  }

  /**
   * Injects the DevTools reload script and configuration into the current page.
   *
   * <p>
   * Builds a WebSocket URL based on the current request, configures the client script with
   * connection parameters, and adds it to the page for immediate execution.
   * </p>
   *
   * @param app the webforJ application instance
   */
  private void injectReloadScript(App app) {
    Page page = Page.getCurrent();
    if (page == null) {
      logger.log(System.Logger.Level.WARNING, "Page.getCurrent() returned null");
      return;
    }

    // Get the current request to determine the WebSocket URL
    Request request = Request.getCurrent();
    if (request == null) {
      logger.log(System.Logger.Level.WARNING, "Request.getCurrent() returned null");
      return;
    }

    String protocol = request.getProtocol();
    String wsProtocol = "https".equals(protocol) ? "wss" : "ws";
    String host = request.getHost();

    // Extract hostname without port if present
    int colonIndex = host.indexOf(':');
    if (colonIndex > 0) {
      host = host.substring(0, colonIndex);
    }

    String websocketPath =
        getProperty("webforj.devtools.reload.websocketPath", "/webforj-devtools-ws");

    // Get the WebSocket port from configuration
    String websocketPort = getProperty("webforj.devtools.reload.websocketPort", "35730");

    // Build WebSocket URL using the separate port
    String wsUrl = String.format("%s://%s:%s%s", wsProtocol, host, websocketPort, websocketPath);
    logger.log(System.Logger.Level.DEBUG, "WebSocket URL: " + wsUrl);
    logger.log(System.Logger.Level.DEBUG, "Request URL: " + request.getUrl());

    // Get heartbeat interval
    String heartbeatInterval = getProperty("webforj.devtools.reload.heartbeatInterval", "30000");

    // Build the complete script with configuration and reload logic
    String config = String.format("window.webforjDevToolsConfig = {" + "  enabled: true,"
        + "  websocketUrl: '%s'," + "  heartbeatInterval: %s," + "  reconnectDelay: 1000,"
        + "  maxReconnectAttempts: 10" + "};", wsUrl, heartbeatInterval);

    String reloadScript = getReloadScript();
    if (reloadScript != null) {
      // Combine configuration and reload script
      String completeScript = config + "\n" + reloadScript;
      page.addInlineJavaScript(completeScript, true);
      logger.log(System.Logger.Level.DEBUG, "webforJ DevTools reload script injected into page");
    } else {
      logger.log(System.Logger.Level.ERROR, "Failed to load webforJ reload script resource");
    }
  }

  /**
   * Loads the DevTools reload client JavaScript from classpath resources.
   *
   * <p>
   * The script is loaded once and cached for subsequent calls. Returns null if the resource cannot
   * be found or read.
   * </p>
   *
   * @return the JavaScript content, or null if loading fails
   */
  private String getReloadScript() {
    if (cachedScript != null) {
      return cachedScript;
    }

    try (InputStream is = getClass().getResourceAsStream(SCRIPT_RESOURCE)) {
      if (is == null) {
        logger.log(System.Logger.Level.ERROR,
            "DevTools reload script not found at: " + SCRIPT_RESOURCE);
        return null;
      }
      cachedScript = new String(is.readAllBytes(), StandardCharsets.UTF_8);
      return cachedScript;
    } catch (Exception e) {
      logger.log(System.Logger.Level.ERROR, "Failed to load webforJ DevTools reload script", e);
    }

    return null;
  }
}
