package com.webforj.spring.devtools;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Spring Boot configuration properties for webforJ DevTools browser reload functionality.
 *
 * <p>
 * These properties control the behavior of the automatic browser reload feature that integrates
 * with Spring Boot DevTools. Properties can be configured in {@code application.properties} or
 * {@code application.yml} under the "webforj.devtools.reload" prefix.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@ConfigurationProperties(prefix = "webforj.devtools.reload")
public class DevToolsReloadProperties {
  /**
   * Enable webforJ DevTools browser auto-reload feature.
   */
  private boolean enabled = false;

  /**
   * WebSocket heartbeat interval in milliseconds.
   */
  private int heartbeatInterval = 30000;

  /**
   * WebSocket endpoint path for DevTools connection.
   */
  private String websocketPath = "/webforj-devtools-ws";

  /**
   * WebSocket server port for DevTools connection.
   */
  private int websocketPort = 35730;

  /**
   * Enable hot reload for static resources (CSS, JS, images). When disabled, falls back to Spring
   * DevTools default behavior.
   */
  private boolean staticResourcesEnabled = true;

  /**
   * Gets whether browser auto-reload is enabled.
   *
   * @return true if enabled, false otherwise
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Sets whether browser auto-reload is enabled.
   *
   * @param enabled true to enable auto-reload, false to disable
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Gets the WebSocket heartbeat interval.
   *
   * @return the interval in milliseconds
   */
  public int getHeartbeatInterval() {
    return heartbeatInterval;
  }

  /**
   * Sets the WebSocket heartbeat interval.
   *
   * @param heartbeatInterval the interval in milliseconds
   */
  public void setHeartbeatInterval(int heartbeatInterval) {
    this.heartbeatInterval = heartbeatInterval;
  }

  /**
   * Gets the WebSocket endpoint path.
   *
   * @return the WebSocket path
   */
  public String getWebsocketPath() {
    return websocketPath;
  }

  /**
   * Sets the WebSocket endpoint path.
   *
   * @param websocketPath the WebSocket path
   */
  public void setWebsocketPath(String websocketPath) {
    this.websocketPath = websocketPath;
  }

  /**
   * Gets the WebSocket server port.
   *
   * @return the port number
   */
  public int getWebsocketPort() {
    return websocketPort;
  }

  /**
   * Sets the WebSocket server port.
   *
   * @param websocketPort the port number
   */
  public void setWebsocketPort(int websocketPort) {
    this.websocketPort = websocketPort;
  }

  /**
   * Gets whether static resource hot reload is enabled.
   *
   * @return true if static resource hot reload is enabled, false otherwise
   */
  public boolean isStaticResourcesEnabled() {
    return staticResourcesEnabled;
  }

  /**
   * Sets whether static resource hot reload is enabled. When disabled, falls back to Spring
   * DevTools default behavior.
   *
   * @param staticResourcesEnabled true to enable static resource hot reload, false to disable
   */
  public void setStaticResourcesEnabled(boolean staticResourcesEnabled) {
    this.staticResourcesEnabled = staticResourcesEnabled;
  }
}
