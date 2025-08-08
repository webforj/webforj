package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.typesafe.config.Config;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.environment.ObjectTable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Context object that carries state through the bootstrap process.
 *
 * <p>
 * The {@code BootstrapContext} provides access to bootstrap parameters, configuration, and allows
 * listeners to share data throughout the bootstrap lifecycle. It serves as the primary
 * communication mechanism between the bootstrap process and its listeners.
 * </p>
 *
 * <p>
 * The context evolves through the bootstrap lifecycle:
 * </p>
 * <ol>
 * <li>Initially contains basic parameters and unique Bootstrap ID</li>
 * <li>After environment preparation, contains loaded configuration</li>
 * <li>After context preparation, contains the detected application entry point</li>
 * <li>Throughout the lifecycle, can store custom attributes shared between listeners</li>
 * </ol>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public final class BootstrapContext {
  private final BBjAPI api;
  private final WebforjBBjBridge bridge;
  private final int debug;
  private final String requestedClassName;
  private final String id;
  private Config configuration;
  private String entry;
  private final Map<String, Object> attributes = new ConcurrentHashMap<>();

  /**
   * Creates a new bootstrap context.
   *
   * @param api the BBjAPI instance
   * @param bridge the WebforjBBjBridge instance
   * @param debug the debug flag (1 for enabled, 0 for disabled)
   * @param requestedClassName the explicitly requested App class name, or null for auto-detection
   * @param id the unique Bootstrap instance ID
   */
  BootstrapContext(BBjAPI api, WebforjBBjBridge bridge, int debug, String requestedClassName,
      String id) {
    this.api = api;
    this.bridge = bridge;
    this.debug = debug;
    this.requestedClassName = requestedClassName;
    this.id = id;
  }

  /**
   * Gets the unique identifier for the Bootstrap instance.
   *
   * @return the Bootstrap ID
   */
  public String getId() {
    return id;
  }

  /**
   * Gets the BBjAPI instance.
   *
   * @return the BBjAPI instance
   */
  public BBjAPI getApi() {
    return api;
  }

  /**
   * Gets the WebforjBBjBridge instance.
   *
   * @return the bridge instance
   */
  public WebforjBBjBridge getBridge() {
    return bridge;
  }

  /**
   * Gets the debug flag.
   *
   * @return 1 if debug mode is enabled, 0 otherwise
   */
  public int getDebug() {
    return debug;
  }

  /**
   * Gets the explicitly requested application class name.
   *
   * @return the requested class name, or null if auto-detection should be used
   */
  public String getRequestedClassName() {
    return requestedClassName;
  }

  /**
   * Gets the current configuration.
   *
   * <p>
   * The configuration is available after the environment has been prepared. Before that point, this
   * method returns null.
   * </p>
   *
   * @return the current configuration, or null if not yet loaded
   */
  public Config getConfiguration() {
    return configuration;
  }

  /**
   * Sets the configuration.
   *
   * @param configuration the new configuration
   */
  void setConfiguration(Config configuration) {
    this.configuration = configuration;
  }

  /**
   * Replaces the current configuration with a new one.
   *
   * @param newConfiguration the new configuration to use
   */
  public void replaceConfiguration(Config newConfiguration) {
    this.configuration = newConfiguration;
    ObjectTable.put("webforj.configuration", newConfiguration);
  }

  /**
   * Merges additional configuration with the existing configuration.
   *
   * <p>
   * The additional configuration takes precedence over existing values. This is useful for
   * overlaying external configuration sources on top of file-based configuration.
   * </p>
   *
   * @param additionalConfig the configuration to merge
   * @return the merged configuration
   */
  public Config mergeConfiguration(Config additionalConfig) {
    if (this.configuration == null) {
      this.configuration = additionalConfig;
    } else {
      this.configuration = additionalConfig.withFallback(this.configuration);
    }
    ObjectTable.put("webforj.configuration", this.configuration);
    return this.configuration;
  }

  /**
   * Gets the application entry point class name.
   *
   * <p>
   * This is available after the context has been prepared and the application entry point has been
   * detected through scanning, configuration, or explicit specification.
   * </p>
   *
   * @return the application entry point class name, or null if not yet detected
   */
  public String getEntry() {
    return entry;
  }

  /**
   * Sets the application entry point class name.
   *
   * @param entry the entry point class name
   */
  void setEntry(String entry) {
    this.entry = entry;
  }

  /**
   * Stores an attribute in the context.
   *
   * <p>
   * Attributes allow listeners to share data with each other throughout the bootstrap lifecycle.
   * This is useful for coordination between multiple listeners or for passing state forward in the
   * process.
   * </p>
   *
   * @param key the attribute key
   * @param value the attribute value
   */
  public void setAttribute(String key, Object value) {
    if (value == null) {
      attributes.remove(key);
    } else {
      attributes.put(key, value);
    }
  }

  /**
   * Retrieves an attribute from the context.
   *
   * @param key the attribute key
   * @return the attribute value, or null if not present
   */
  public Object getAttribute(String key) {
    return attributes.get(key);
  }

  /**
   * Retrieves a typed attribute from the context.
   *
   * @param <T> the expected type
   * @param key the attribute key
   * @param type the expected class type
   * @return the attribute value cast to the specified type, or null if not present or wrong type
   */
  public <T> T getAttribute(String key, Class<T> type) {
    Object value = attributes.get(key);
    if (type.isInstance(value)) {
      return type.cast(value);
    }
    return null;
  }

  /**
   * Checks if an attribute exists in the context.
   *
   * @param key the attribute key
   * @return true if the attribute exists, false otherwise
   */
  public boolean hasAttribute(String key) {
    return attributes.containsKey(key);
  }
}
