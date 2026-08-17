package com.webforj.mcp;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import java.io.File;
import java.util.List;

/**
 * The configuration that drives the MCP apps support.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpAppOptions {

  /**
   * The configuration key for the public origin the host loads the application from.
   *
   * <p>
   * The key is the framework's own {@code webforj.origin}, read here so the MCP support and the
   * framework resolve the same address.
   * </p>
   */
  public static final String KEY_ORIGIN = "webforj.origin";

  /**
   * The configuration key for the origins allowed to embed the application.
   */
  public static final String KEY_ALLOWED_ORIGINS = "webforj.mcp.allowed-origins";

  /**
   * The configuration key for origins the embedding frame may load resources from in addition to
   * the application origin and the framework domains.
   */
  public static final String KEY_RESOURCE_DOMAINS = "webforj.mcp.resource-domains";

  /**
   * The configuration key for origins the embedding frame may connect to in addition to the
   * application origin and the framework domains.
   */
  public static final String KEY_CONNECT_DOMAINS = "webforj.mcp.connect-domains";

  private static final String CONFIG_PROPERTY = "webforj.conf";
  private static final String RESOURCE_PREFIX = "!!";
  private static final String DEFAULT_CONFIG = "!!webforj.conf";
  private static final String COMPONENTS_KEY = "webforj.components";

  private String origin;
  private List<String> allowedOrigins = List.of();
  private List<String> resourceDomains = List.of();
  private List<String> connectDomains = List.of();
  private String components;

  /**
   * Reads the options of a plain deployment, system properties overriding {@code webforj.conf}.
   *
   * <p>
   * The configuration file is the one the framework itself reads, located through the
   * {@code webforj.conf} system property when set, the {@code webforj.conf} classpath resource
   * otherwise. The file exists from the moment the deployment does, so nothing waits for the
   * application to run.
   * </p>
   *
   * @return the options filled from the deployment configuration
   */
  public static McpAppOptions load() {
    // The properties are parsed on every call rather than taken from the cached snapshot the
    // factory keeps, so a property set after the first read is still honored.
    return from(
        ConfigFactory.parseProperties(System.getProperties()).withFallback(deploymentConfig()));
  }

  /**
   * Reads the options from the given configuration.
   *
   * @param config the configuration, may be {@code null}
   * @return the options filled from the configuration, defaults where a key is absent
   */
  public static McpAppOptions from(Config config) {
    return new McpAppOptions().setOrigin(getString(config, KEY_ORIGIN, null))
        .setAllowedOrigins(getStrings(config, KEY_ALLOWED_ORIGINS))
        .setResourceDomains(getStrings(config, KEY_RESOURCE_DOMAINS))
        .setConnectDomains(getStrings(config, KEY_CONNECT_DOMAINS))
        .setComponents(getString(config, COMPONENTS_KEY, null));
  }

  /**
   * Sets the public origin the host loads the application from.
   *
   * @param origin the public origin
   * @return this options instance
   */
  public McpAppOptions setOrigin(String origin) {
    this.origin = origin == null || origin.isBlank() ? null : origin.trim();
    return this;
  }

  /**
   * Gets the public origin the host loads the application from.
   *
   * @return the public origin, {@code null} when unset
   */
  public String getOrigin() {
    return origin;
  }

  /**
   * Sets the origins allowed to embed the application beside the known MCP hosts.
   *
   * @param allowedOrigins the additional allowed origins, an empty list adding none
   * @return this options instance
   */
  public McpAppOptions setAllowedOrigins(List<String> allowedOrigins) {
    this.allowedOrigins = allowedOrigins == null ? List.of() : List.copyOf(allowedOrigins);
    return this;
  }

  /**
   * Gets the origins allowed to embed the application beside the known MCP hosts.
   *
   * @return the additional allowed origins, an empty list adding none
   */
  public List<String> getAllowedOrigins() {
    return allowedOrigins;
  }

  /**
   * Sets the origins the embedding frame may load resources from in addition to the application
   * origin and the framework domains.
   *
   * @param resourceDomains the additional resource origins, an empty list adding none
   * @return this options instance
   */
  public McpAppOptions setResourceDomains(List<String> resourceDomains) {
    this.resourceDomains = resourceDomains == null ? List.of() : List.copyOf(resourceDomains);
    return this;
  }

  /**
   * Gets the origins the embedding frame may load resources from in addition to the application
   * origin and the framework domains.
   *
   * @return the additional resource origins, an empty list adding none
   */
  public List<String> getResourceDomains() {
    return resourceDomains;
  }

  /**
   * Sets the origins the embedding frame may connect to in addition to the application origin and
   * the framework domains.
   *
   * @param connectDomains the additional connect origins, an empty list adding none
   * @return this options instance
   */
  public McpAppOptions setConnectDomains(List<String> connectDomains) {
    this.connectDomains = connectDomains == null ? List.of() : List.copyOf(connectDomains);
    return this;
  }

  /**
   * Gets the origins the embedding frame may connect to in addition to the application origin and
   * the framework domains.
   *
   * @return the additional connect origins, an empty list adding none
   */
  public List<String> getConnectDomains() {
    return connectDomains;
  }

  /**
   * Sets the component library address the deployment declares.
   *
   * <p>
   * The key is the framework's own {@code webforj.components}. It is carried here so an explicit
   * value always wins over the address derived from the origin.
   * </p>
   *
   * @param components the component library address
   * @return this options instance
   */
  public McpAppOptions setComponents(String components) {
    this.components = components == null || components.isBlank() ? null : components.trim();
    return this;
  }

  /**
   * Gets the component library address the deployment declares.
   *
   * @return the component library address, {@code null} when unset
   */
  public String getComponents() {
    return components;
  }

  private static Config deploymentConfig() {
    String pathProperty = System.getProperty(CONFIG_PROPERTY, DEFAULT_CONFIG);
    if (pathProperty.startsWith(RESOURCE_PREFIX)) {
      return ConfigFactory.parseResourcesAnySyntax(Thread.currentThread().getContextClassLoader(),
          pathProperty.substring(RESOURCE_PREFIX.length()));
    }

    return ConfigFactory.parseFile(new File(pathProperty));
  }

  private static String getString(Config config, String key, String fallback) {
    if (config != null && config.hasPath(key) && !config.getIsNull(key)) {
      return config.getString(key);
    }

    return fallback;
  }

  private static List<String> getStrings(Config config, String key) {
    if (config == null || !config.hasPath(key) || config.getIsNull(key)) {
      return List.of();
    }

    try {
      return List.copyOf(config.getStringList(key));
    } catch (ConfigException.WrongType e) {
      return splitCommaList(config.getString(key));
    }
  }

  private static List<String> splitCommaList(String value) {
    if (value == null || value.isBlank()) {
      return List.of();
    }

    return List.of(value.split(",")).stream().map(String::trim).filter(entry -> !entry.isEmpty())
        .toList();
  }
}
