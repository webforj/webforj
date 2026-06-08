package com.webforj.devtools.livereload;

import com.typesafe.config.Config;

/**
 * The configuration that drives the development live reload.
 *
 * <p>
 * The values carry the reload socket port and path, the client heartbeat interval, whether live
 * reload is on at all, and whether a stylesheet or image change is hot swapped rather than
 * reloaded. Each runtime fills these from its own configuration source and hands them to the live
 * reload lifecycle and the page injector, so the keys and the defaults are defined in one place.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class LiveReloadOptions {

  /**
   * The configuration key that turns live reload on.
   */
  public static final String KEY_ENABLED = "webforj.devtools.livereload.enabled";

  /**
   * The configuration key for the reload socket port.
   */
  public static final String KEY_WEBSOCKET_PORT = "webforj.devtools.livereload.websocket-port";

  /**
   * The configuration key for the reload socket path.
   */
  public static final String KEY_WEBSOCKET_PATH = "webforj.devtools.livereload.websocket-path";

  /**
   * The configuration key for the client heartbeat interval in milliseconds.
   */
  public static final String KEY_HEARTBEAT_INTERVAL =
      "webforj.devtools.livereload.heartbeat-interval";

  /**
   * The configuration key that allows a stylesheet or image change to hot swap rather than reload.
   */
  public static final String KEY_STATIC_RESOURCES_ENABLED =
      "webforj.devtools.livereload.static-resources-enabled";

  /**
   * The default reload socket port.
   */
  public static final int DEFAULT_WEBSOCKET_PORT = 35730;

  /**
   * The default reload socket path.
   */
  public static final String DEFAULT_WEBSOCKET_PATH = "/webforj-devtools-ws";

  /**
   * The default client heartbeat interval in milliseconds.
   */
  public static final int DEFAULT_HEARTBEAT_INTERVAL = 30000;

  private boolean enabled = false;
  private int websocketPort = DEFAULT_WEBSOCKET_PORT;
  private String websocketPath = DEFAULT_WEBSOCKET_PATH;
  private int heartbeatInterval = DEFAULT_HEARTBEAT_INTERVAL;
  private boolean staticResourcesEnabled = true;

  /**
   * Reads the options from the given webforJ configuration.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @return the options filled from the configuration, defaults where a key is absent
   */
  public static LiveReloadOptions from(Config config) {
    return new LiveReloadOptions().setEnabled(getBoolean(config, KEY_ENABLED, false))
        .setWebsocketPort(getInt(config, KEY_WEBSOCKET_PORT, DEFAULT_WEBSOCKET_PORT))
        .setWebsocketPath(getString(config, KEY_WEBSOCKET_PATH, DEFAULT_WEBSOCKET_PATH))
        .setHeartbeatInterval(getInt(config, KEY_HEARTBEAT_INTERVAL, DEFAULT_HEARTBEAT_INTERVAL))
        .setStaticResourcesEnabled(getBoolean(config, KEY_STATIC_RESOURCES_ENABLED, true));
  }

  /**
   * Sets whether live reload is on.
   *
   * @param enabled {@code true} to turn live reload on
   * @return this options instance
   */
  public LiveReloadOptions setEnabled(boolean enabled) {
    this.enabled = enabled;

    return this;
  }

  /**
   * Indicates whether live reload is on.
   *
   * @return {@code true} when live reload is on
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Sets the reload socket port.
   *
   * @param websocketPort the reload socket port
   * @return this options instance
   */
  public LiveReloadOptions setWebsocketPort(int websocketPort) {
    this.websocketPort = websocketPort;

    return this;
  }

  /**
   * Gets the reload socket port.
   *
   * @return the reload socket port
   */
  public int getWebsocketPort() {
    return websocketPort;
  }

  /**
   * Sets the reload socket path.
   *
   * @param websocketPath the reload socket path
   * @return this options instance
   */
  public LiveReloadOptions setWebsocketPath(String websocketPath) {
    this.websocketPath = websocketPath;

    return this;
  }

  /**
   * Gets the reload socket path.
   *
   * @return the reload socket path
   */
  public String getWebsocketPath() {
    return websocketPath;
  }

  /**
   * Sets the client heartbeat interval in milliseconds.
   *
   * @param heartbeatInterval the heartbeat interval in milliseconds
   * @return this options instance
   */
  public LiveReloadOptions setHeartbeatInterval(int heartbeatInterval) {
    this.heartbeatInterval = heartbeatInterval;

    return this;
  }

  /**
   * Gets the client heartbeat interval in milliseconds.
   *
   * @return the heartbeat interval in milliseconds
   */
  public int getHeartbeatInterval() {
    return heartbeatInterval;
  }

  /**
   * Sets whether a stylesheet or image change hot swaps rather than reloads.
   *
   * @param staticResourcesEnabled {@code true} to hot swap a stylesheet or image change
   * @return this options instance
   */
  public LiveReloadOptions setStaticResourcesEnabled(boolean staticResourcesEnabled) {
    this.staticResourcesEnabled = staticResourcesEnabled;

    return this;
  }

  /**
   * Indicates whether a stylesheet or image change hot swaps rather than reloads.
   *
   * @return {@code true} when a stylesheet or image change hot swaps
   */
  public boolean isStaticResourcesEnabled() {
    return staticResourcesEnabled;
  }

  private static boolean getBoolean(Config config, String key, boolean defaultValue) {
    if (config != null && config.hasPath(key) && !config.getIsNull(key)) {
      return config.getBoolean(key);
    }

    return defaultValue;
  }

  private static int getInt(Config config, String key, int defaultValue) {
    if (config != null && config.hasPath(key) && !config.getIsNull(key)) {
      return config.getInt(key);
    }

    return defaultValue;
  }

  private static String getString(Config config, String key, String defaultValue) {
    if (config != null && config.hasPath(key) && !config.getIsNull(key)) {
      return config.getString(key);
    }

    return defaultValue;
  }
}
