package com.webforj.spring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Webforj boot configuration properties.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@ConfigurationProperties(prefix = "webforj")
public class SpringConfigurationProperties {

  /**
   * The URL mapping for the Webforj servlet.
   *
   * <p>
   * This property defaults to "/*" if not specified.
   * </p>
   */
  private String servletMapping = "/*";

  /**
   * The application's entry point class.
   *
   * <p>
   * Define the application's entry point by specifying the fully qualified name of the class that
   * extends {@code com.webforj.App}. If no entry point is defined, webforJ will automatically scan
   * the classpath for classes that extend {@code com.webforj.App}. If multiple classes are found,
   * an error will occur. When a package includes more than one potential entry point, setting this
   * explicitly is required to prevent ambiguity, or alternatively, the {@code AppEntry} annotation
   * can be used to specify the entry point at runtime.
   * </p>
   */
  private String entry;

  /**
   * Debug mode configuration.
   *
   * <p>
   * Set to true to enable debug mode. In debug mode, webforJ will print additional information to
   * the console and show all exceptions in the browser. Debug mode is disabled by default.
   * </p>
   */
  private Boolean debug;

  /**
   * Base path for loading DWC components.
   *
   * <p>
   * When specified, the base path determines where DWC components are loaded from. By default,
   * components are loaded from the server hosting the application. However, setting a custom base
   * path allows components to be loaded from an alternate server or CDN. For example, to load
   * components from jsdelivr.com, set the base path to:
   * {@code https://cdn.jsdelivr.net/gh/webforj/dwc-dist@1.0.0-${webforj.version}}
   * </p>
   *
   * <p>
   * It's important that the loaded components are compatible with the version of the webforJ
   * framework in use; otherwise, the application may not function as expected.
   * </p>
   */
  private String components;

  /**
   * Application locale configuration.
   *
   * <p>
   * Define the locale for the application. The locale determines the language and region settings
   * used by the application as well as the format of dates, times, and numbers.
   * </p>
   */
  private String locale;

  /**
   * Controls whether to display the loading image.
   *
   * <p>
   * Set to {@code true} to disable the loading image during application startup.
   * </p>
   */
  private Boolean quiet;

  /**
   * When using hot redeploy, the whole WAR file will be swapped.
   *
   * <p>
   * If the client tries to send a request to the server while it is restarting, an error occurs.
   * This setting allows the client to attempt a page reload if the server is temporarily
   * unavailable, hoping it will be back online shortly. This only applies to development
   * environments and only handles errors specific to hot redeployment, not other types of errors.
   * </p>
   */
  private Boolean reloadOnServerError;

  /**
   * Sets the interval at which the client pings the server to see if it's still alive.
   *
   * <p>
   * This helps maintain communication. For development, set this to a shorter interval, for example
   * {@code 8s}, to quickly detect server availability. For production environments, it's
   * recommended to use values of 50 seconds or higher to minimize server load.
   * </p>
   */
  private String clientHeartbeatRate;

  /**
   * Session timeout in seconds.
   *
   * <p>
   * Sets the session timeout duration in seconds. If not specified, defaults to 60 seconds. This
   * controls how long a session remains active without any client activity before being terminated.
   * </p>
   */
  private Integer sessionTimeout;

  /**
   * Specifies the route name used to serve static files.
   *
   * <p>
   * While the physical folder name remains {@code static}, this configuration is helpful if the
   * default static route conflicts with a route defined in your app, allowing you to change the
   * route name without renaming the folder itself.
   * </p>
   */
  private String assetsDir;

  /**
   * Cache control header for static assets.
   *
   * <p>
   * Sets the {@code Cache-Control} header for static resources. For example:
   * {@code max-age=3600, public} or {@code no-cache}.
   * </p>
   */
  private String assetsCacheControl;

  /**
   * Default file extension for static files.
   *
   * <p>
   * The file extension to use when serving files from the static folder.
   * </p>
   */
  private String assetsExt;

  /**
   * Index file for static assets.
   *
   * <p>
   * The default file to serve when a directory is requested from the static assets folder. For
   * example, {@code index.html}.
   * </p>
   */
  private String assetsIndex;

  /**
   * Icons directory endpoint name.
   *
   * <p>
   * By default, webforJ serves icons from the {@code resources/icons/} directory. This property
   * changes the URL endpoint used to access the icons, not the actual folder name. The default
   * endpoint is {@code icons/}.
   * </p>
   */
  private String iconsDir;

  /**
   * String table for application strings.
   *
   * <p>
   * The string table is a map of key-value pairs that can be used to store strings for use in the
   * application.
   * </p>
   *
   * @see <a href=
   *      "https://docs.webforj.com/docs/advanced/object-string-tables#stringtable">StringTable</a>
   */
  private Map<String, String> stringTable = new HashMap<>();

  /**
   * File upload configuration.
   */
  private FileUpload fileUpload = new FileUpload();

  /**
   * Servlet configurations for user-provided servlets.
   *
   * <p>
   * Configure additional servlets that will be managed by webforJ. Each servlet can have its own
   * class, name, and initialization parameters.
   * </p>
   */
  private List<ServletConfig> servlets = new ArrayList<>();

  /**
   * License configuration.
   */
  private License license = new License();

  /**
   * URL patterns that should not be handled by webforJ when mapped to root.
   *
   * <p>
   * When webforJ is mapped to the root context ({@code /*}), these URL patterns will be excluded
   * from webforJ handling and can be handled by Spring MVC controllers instead. This allows REST
   * endpoints and other Spring MVC mappings to coexist with webforJ routes.
   * </p>
   *
   * <p>
   * Example patterns:
   * <ul>
   * <li>{@code /api/**} - All paths under /api</li>
   * <li>{@code /actuator/**} - Spring Boot actuator endpoints</li>
   * </ul>
   * </p>
   *
   * @since 25.03
   */
  private List<String> excludeUrls = new ArrayList<>();

  /**
   * MIME type configuration.
   *
   * @since 25.10
   */
  private Mime mime = new Mime();

  /**
   * Sets the URL mapping for the Webforj servlet.
   *
   * @param servletMapping the URL mapping for the Webforj servlet
   */
  public void setServletMapping(String servletMapping) {
    this.servletMapping = servletMapping;
  }

  /**
   * Gets the URL mapping for the Webforj servlet.
   *
   * @return the URL mapping for the Webforj servlet
   */
  public String getServletMapping() {
    return servletMapping;
  }

  /**
   * Gets the application entry point.
   *
   * @return the fully qualified class name of the entry point
   */
  public String getEntry() {
    return entry;
  }

  /**
   * Sets the application entry point.
   *
   * @param entry the fully qualified class name of the entry point
   */
  public void setEntry(String entry) {
    this.entry = entry;
  }

  /**
   * Gets the debug mode setting.
   *
   * @return true if debug mode is enabled, false otherwise, null if not set
   */
  public Boolean getDebug() {
    return debug;
  }

  /**
   * Sets the debug mode.
   *
   * @param debug true to enable debug mode, false to disable
   */
  public void setDebug(Boolean debug) {
    this.debug = debug;
  }

  /**
   * Gets the components base path.
   *
   * @return the base path for loading DWC components
   */
  public String getComponents() {
    return components;
  }

  /**
   * Sets the components base path.
   *
   * @param components the base path for loading DWC components
   */
  public void setComponents(String components) {
    this.components = components;
  }

  /**
   * Gets the application locale.
   *
   * @return the locale string
   */
  public String getLocale() {
    return locale;
  }

  /**
   * Sets the application locale.
   *
   * @param locale the locale string
   */
  public void setLocale(String locale) {
    this.locale = locale;
  }

  /**
   * Gets the string table.
   *
   * @return the string table map
   */
  public Map<String, String> getStringTable() {
    return stringTable;
  }

  /**
   * Sets the string table.
   *
   * @param stringTable the string table map
   */
  public void setStringTable(Map<String, String> stringTable) {
    this.stringTable = stringTable;
  }

  /**
   * Gets the file upload configuration.
   *
   * @return the file upload configuration
   */
  public FileUpload getFileUpload() {
    return fileUpload;
  }

  /**
   * Sets the file upload configuration.
   *
   * @param fileUpload the file upload configuration
   */
  public void setFileUpload(FileUpload fileUpload) {
    this.fileUpload = fileUpload;
  }

  /**
   * Gets the reload on server error setting.
   *
   * @return true if reload on server error is enabled, false otherwise, null if not set
   */
  public Boolean getReloadOnServerError() {
    return reloadOnServerError;
  }

  /**
   * Sets the reload on server error setting.
   *
   * @param reloadOnServerError true to enable reload on server error, false to disable
   */
  public void setReloadOnServerError(Boolean reloadOnServerError) {
    this.reloadOnServerError = reloadOnServerError;
  }

  /**
   * Gets the client heartbeat rate.
   *
   * @return the client heartbeat rate interval
   */
  public String getClientHeartbeatRate() {
    return clientHeartbeatRate;
  }

  /**
   * Sets the client heartbeat rate.
   *
   * @param clientHeartbeatRate the client heartbeat rate interval
   */
  public void setClientHeartbeatRate(String clientHeartbeatRate) {
    this.clientHeartbeatRate = clientHeartbeatRate;
  }

  /**
   * Gets the assets directory route name.
   *
   * @return the assets directory route name
   */
  public String getAssetsDir() {
    return assetsDir;
  }

  /**
   * Sets the assets directory route name.
   *
   * @param assetsDir the assets directory route name
   */
  public void setAssetsDir(String assetsDir) {
    this.assetsDir = assetsDir;
  }

  /**
   * Gets the quiet mode setting.
   *
   * @return true if loading image is disabled, false otherwise, null if not set
   */
  public Boolean getQuiet() {
    return quiet;
  }

  /**
   * Sets the quiet mode.
   *
   * @param quiet true to disable loading image, false to enable
   */
  public void setQuiet(Boolean quiet) {
    this.quiet = quiet;
  }

  /**
   * Gets the assets cache control header.
   *
   * @return the cache control header value
   */
  public String getAssetsCacheControl() {
    return assetsCacheControl;
  }

  /**
   * Sets the assets cache control header.
   *
   * @param assetsCacheControl the cache control header value
   */
  public void setAssetsCacheControl(String assetsCacheControl) {
    this.assetsCacheControl = assetsCacheControl;
  }

  /**
   * Gets the assets file extension.
   *
   * @return the default file extension
   */
  public String getAssetsExt() {
    return assetsExt;
  }

  /**
   * Sets the assets file extension.
   *
   * @param assetsExt the default file extension
   */
  public void setAssetsExt(String assetsExt) {
    this.assetsExt = assetsExt;
  }

  /**
   * Gets the assets index file.
   *
   * @return the index file name
   */
  public String getAssetsIndex() {
    return assetsIndex;
  }

  /**
   * Sets the assets index file.
   *
   * @param assetsIndex the index file name
   */
  public void setAssetsIndex(String assetsIndex) {
    this.assetsIndex = assetsIndex;
  }

  /**
   * Gets the icons directory endpoint name.
   *
   * @return the icons endpoint name
   */
  public String getIconsDir() {
    return iconsDir;
  }

  /**
   * Sets the icons directory endpoint name.
   *
   * @param iconsDir the icons endpoint name
   */
  public void setIconsDir(String iconsDir) {
    this.iconsDir = iconsDir;
  }

  /**
   * Gets the session timeout in seconds.
   *
   * @return the session timeout in seconds
   */
  public Integer getSessionTimeout() {
    return sessionTimeout;
  }

  /**
   * Sets the session timeout in seconds.
   *
   * @param sessionTimeout the session timeout in seconds
   */
  public void setSessionTimeout(Integer sessionTimeout) {
    this.sessionTimeout = sessionTimeout;
  }

  /**
   * Gets the servlet configurations.
   *
   * @return the list of servlet configurations
   */
  public List<ServletConfig> getServlets() {
    return servlets;
  }

  /**
   * Sets the servlet configurations.
   *
   * @param servlets the list of servlet configurations
   */
  public void setServlets(List<ServletConfig> servlets) {
    this.servlets = servlets;
  }

  /**
   * Gets the license configuration.
   *
   * @return the license configuration
   */
  public License getLicense() {
    return license;
  }

  /**
   * Sets the license configuration.
   *
   * @param license the license configuration
   */
  public void setLicense(License license) {
    this.license = license;
  }

  /**
   * Gets the excluded URL patterns.
   *
   * @return the list of URL patterns to exclude from webforJ handling
   */
  public List<String> getExcludeUrls() {
    return excludeUrls;
  }

  /**
   * Sets the excluded URL patterns.
   *
   * @param excludeUrls the list of URL patterns to exclude from webforJ handling
   */
  public void setExcludeUrls(List<String> excludeUrls) {
    this.excludeUrls = excludeUrls;
  }

  /**
   * Gets the MIME type configuration.
   *
   * @return the MIME type configuration
   * @since 25.10
   */
  public Mime getMime() {
    return mime;
  }

  /**
   * Sets the MIME type configuration.
   *
   * @param mime the MIME type configuration
   * @since 25.10
   */
  public void setMime(Mime mime) {
    this.mime = mime;
  }

  /**
   * File upload configuration properties.
   */
  public static class FileUpload {
    /**
     * Allowed file types for file uploads.
     *
     * <p>
     * The allowed file types for file uploads. By default, all file types are allowed.
     * </p>
     *
     * <p>
     * Example values:
     * <ul>
     * <li>{@code image/*}</li>
     * <li>{@code application/pdf}</li>
     * <li>{@code text/plain}</li>
     * <li>{@code *.txt}</li>
     * </ul>
     * </p>
     */
    private List<String> accept = new ArrayList<>();

    /**
     * Maximum file size for uploads in bytes.
     *
     * <p>
     * The maximum file size allowed for file uploads, in bytes. By default, there is no limit on
     * the file size.
     * </p>
     */
    private Long maxSize;

    /**
     * Gets the accepted file types.
     *
     * @return list of accepted file types
     */
    public List<String> getAccept() {
      return accept;
    }

    /**
     * Sets the accepted file types.
     *
     * @param accept list of accepted file types
     */
    public void setAccept(List<String> accept) {
      this.accept = accept;
    }

    /**
     * Gets the maximum file size.
     *
     * @return maximum file size in bytes
     */
    public Long getMaxSize() {
      return maxSize;
    }

    /**
     * Sets the maximum file size.
     *
     * @param maxSize maximum file size in bytes
     */
    public void setMaxSize(Long maxSize) {
      this.maxSize = maxSize;
    }
  }

  /**
   * Servlet configuration properties.
   */
  public static class ServletConfig {
    /**
     * The fully qualified class name of the servlet.
     */
    private String className;

    /**
     * The name of the servlet. If not specified, the class name will be used.
     */
    private String name;

    /**
     * Initialization parameters for the servlet.
     */
    private Map<String, String> config = new HashMap<>();

    /**
     * Gets the servlet class name.
     *
     * @return the fully qualified class name
     */
    public String getClassName() {
      return className;
    }

    /**
     * Sets the servlet class name.
     *
     * @param className the fully qualified class name
     */
    public void setClassName(String className) {
      this.className = className;
    }

    /**
     * Gets the servlet name.
     *
     * @return the servlet name
     */
    public String getName() {
      return name;
    }

    /**
     * Sets the servlet name.
     *
     * @param name the servlet name
     */
    public void setName(String name) {
      this.name = name;
    }

    /**
     * Gets the servlet initialization parameters.
     *
     * @return the initialization parameters map
     */
    public Map<String, String> getConfig() {
      return config;
    }

    /**
     * Sets the servlet initialization parameters.
     *
     * @param config the initialization parameters map
     */
    public void setConfig(Map<String, String> config) {
      this.config = config;
    }
  }

  /**
   * License configuration properties.
   */
  public static class License {
    /**
     * Path to the license configuration dir.
     */
    private String cfg;

    /**
     * License startup timeout.
     */
    private Integer startupTimeout;

    /**
     * Gets the license configuration directory path.
     *
     * @return the license configuration directory path
     */
    public String getCfg() {
      return cfg;
    }

    /**
     * Sets the license configuration directory path.
     *
     * @param cfg the license configuration directory path
     */
    public void setCfg(String cfg) {
      this.cfg = cfg;
    }

    /**
     * Gets the license startup timeout.
     *
     * @return the startup timeout
     */
    public Integer getStartupTimeout() {
      return startupTimeout;
    }

    /**
     * Sets the license startup timeout.
     *
     * @param startupTimeout the startup timeout
     */
    public void setStartupTimeout(Integer startupTimeout) {
      this.startupTimeout = startupTimeout;
    }
  }

  /**
   * MIME type configuration for static files.
   *
   * @since 25.10
   */
  public static class Mime {
    /**
     * Custom MIME type mappings for file extensions.
     *
     * <p>
     * Maps file extensions to their corresponding MIME types. This allows overriding default MIME
     * type mappings or adding new ones.
     * </p>
     *
     * <p>
     * Example configuration:
     *
     * <pre>
     * webforj.mime.extensions.txt = text/plain
     * webforj.mime.extensions.foo = foo/bar
     * webforj.mime.extensions.json = application/json
     * </pre>
     * </p>
     */
    private Map<String, String> extensions = new HashMap<>();

    /**
     * Gets the extension to MIME type mappings.
     *
     * @return the map of file extensions to MIME types
     */
    public Map<String, String> getExtensions() {
      return extensions;
    }

    /**
     * Sets the extension to MIME type mappings.
     *
     * @param extensions the map of file extensions to MIME types
     */
    public void setExtensions(Map<String, String> extensions) {
      this.extensions = extensions;
    }
  }
}
