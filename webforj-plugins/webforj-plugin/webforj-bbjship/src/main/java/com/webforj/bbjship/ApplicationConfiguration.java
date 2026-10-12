package com.webforj.bbjship;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigValue;
import com.webforj.bbjship.DeploymentException.Reason;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Holds the settings an application carries in the {@code webforj.conf} of its application jar or
 * class folders.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ApplicationConfiguration {

  private static final String CONFIGURATION = "webforj.conf";
  private static final String ENTRY = "webforj.entry";
  private static final String ASSETS_DIR = "webforj.assetsDir";
  private static final String ICONS_DIR = "webforj.iconsDir";
  private static final String ASSETS_INDEX = "webforj.assetsIndex";
  private static final String ASSETS_EXT = "webforj.assetsExt";
  private static final String ASSETS_CACHE_CONTROL = "webforj.assetsCacheControl";
  private static final String MIME_EXTENSIONS = "webforj.mime.extensions";
  private static final String QUIET = "webforj.quiet";
  private static final String SHOW_BROWSER_WARNING = "webforj.showBrowserWarning";
  private static final String MANAGE_BROWSER_HISTORY = "webforj.manageBrowserHistory";
  private static final String SHOW_CLIENT_CONFIRMATION_DIALOG =
      "webforj.showClientConfirmationDialog";
  private static final String DISALLOW_CONSOLE = "webforj.disallowConsole";
  private static final String SECURE = "webforj.secure";
  private static final String CONFIG_FILE = "webforj.configFile";
  private static final String PROGRAM_ARGUMENTS = "webforj.programArguments";

  private final String entryClass;
  private final String assetsPrefix;
  private final String iconsPrefix;
  private final List<String> indexes;
  private final List<String> extensions;
  private final String cacheControl;
  private final Map<String, String> mimeExtensions;
  private final Boolean quietMode;
  private final Boolean showBrowserWarning;
  private final Boolean manageBrowserHistory;
  private final boolean clientConfirmationDialogShown;
  private final boolean consoleDisallowed;
  private final Boolean secureSetting;
  private final String configFile;
  private final List<String> programArguments;

  private ApplicationConfiguration(Config configuration) {
    this.entryClass = configuration.hasPath(ENTRY) ? configuration.getString(ENTRY) : null;
    this.assetsPrefix = readAssetsPrefix(configuration);
    this.iconsPrefix = readPrefix(configuration, ICONS_DIR, "icons/");
    this.indexes = List.copyOf(readStrings(configuration, ASSETS_INDEX, List.of("index.html")));
    List<String> configuredExtensions = readStrings(configuration, ASSETS_EXT, List.of(""));
    if (!configuredExtensions.contains("")) {
      configuredExtensions = new ArrayList<>(configuredExtensions);
      configuredExtensions.add(0, "");
    }

    this.extensions = List.copyOf(configuredExtensions);
    this.cacheControl =
        configuration.hasPath(ASSETS_CACHE_CONTROL) ? configuration.getString(ASSETS_CACHE_CONTROL)
            : null;
    this.mimeExtensions = readMimeExtensions(configuration);
    this.quietMode = readBoolean(configuration, QUIET);
    this.showBrowserWarning = readBoolean(configuration, SHOW_BROWSER_WARNING);
    this.manageBrowserHistory = readBoolean(configuration, MANAGE_BROWSER_HISTORY);
    this.clientConfirmationDialogShown = configuration.hasPath(SHOW_CLIENT_CONFIRMATION_DIALOG)
        && configuration.getBoolean(SHOW_CLIENT_CONFIRMATION_DIALOG);
    this.consoleDisallowed =
        !configuration.hasPath(DISALLOW_CONSOLE) || configuration.getBoolean(DISALLOW_CONSOLE);
    this.secureSetting = readBoolean(configuration, SECURE);
    this.configFile =
        configuration.hasPath(CONFIG_FILE) ? configuration.getString(CONFIG_FILE) : null;
    this.programArguments = List.copyOf(readStrings(configuration, PROGRAM_ARGUMENTS, List.of()));
  }

  /**
   * Loads the configuration of an application jar or of class folders.
   *
   * <p>
   * The {@code webforj.conf} of the first source that has one is read. A source that does not exist
   * holds no configuration.
   * </p>
   *
   * @param sources the application jar or the class folders in classpath order
   *
   * @return the configuration, with defaults when no source has a {@code webforj.conf}
   * @throws DeploymentException if the {@code webforj.conf} that is read is invalid
   * @throws IOException if a source or its configuration cannot be read
   */
  public static ApplicationConfiguration load(Path... sources) throws IOException {
    Config configuration = ConfigFactory.empty();
    for (Path source : sources) {
      Optional<Config> found = readSource(source);
      if (found.isPresent()) {
        configuration = found.get();
        break;
      }
    }

    try {
      return new ApplicationConfiguration(configuration);
    } catch (ConfigException invalid) {
      throw createInvalidFailure(invalid);
    }
  }

  /**
   * Gets the entry class of the application.
   *
   * @return the entry class or empty when it is not configured
   */
  public Optional<String> getEntryClass() {
    return Optional.ofNullable(entryClass);
  }

  /**
   * Gets the resource prefix of the static assets.
   *
   * @return the assets prefix ending with a slash, empty when the assets are at the jar root
   */
  public String getAssetsPrefix() {
    return assetsPrefix;
  }

  /**
   * Gets the resource prefix of the icons.
   *
   * @return the icons prefix ending with a slash
   */
  public String getIconsPrefix() {
    return iconsPrefix;
  }

  /**
   * Gets the index file names of a directory request.
   *
   * @return the index file names
   */
  public List<String> getIndexes() {
    return indexes;
  }

  /**
   * Gets the extensions tried for an asset request, starting with the empty extension.
   *
   * @return the extensions
   */
  public List<String> getExtensions() {
    return extensions;
  }

  /**
   * Gets the cache control value of the assets.
   *
   * @return the cache control value or null when none is configured
   */
  public String getCacheControl() {
    return cacheControl;
  }

  /**
   * Gets the content types by file extension.
   *
   * @return the content types by file extension
   */
  public Map<String, String> getMimeExtensions() {
    return mimeExtensions;
  }

  /**
   * Gets the quiet setting.
   *
   * @return the quiet setting or empty when it is not configured
   */
  public Optional<Boolean> getQuietMode() {
    return Optional.ofNullable(quietMode);
  }

  /**
   * Gets the browser warning setting.
   *
   * @return the browser warning setting or empty when it is not configured
   */
  public Optional<Boolean> getShowBrowserWarning() {
    return Optional.ofNullable(showBrowserWarning);
  }

  /**
   * Gets the browser history setting.
   *
   * @return the browser history setting or empty when it is not configured
   */
  public Optional<Boolean> getManageBrowserHistory() {
    return Optional.ofNullable(manageBrowserHistory);
  }

  /**
   * Checks whether the client confirmation dialog is shown.
   *
   * @return true if the dialog is shown, false by default
   */
  public boolean isClientConfirmationDialogShown() {
    return clientConfirmationDialogShown;
  }

  /**
   * Checks whether the console is disallowed.
   *
   * @return true if the console is disallowed, true by default
   */
  public boolean isConsoleDisallowed() {
    return consoleDisallowed;
  }

  /**
   * Gets the secure setting.
   *
   * @return the secure setting or empty when it is not configured
   */
  public Optional<Boolean> getSecure() {
    return Optional.ofNullable(secureSetting);
  }

  /**
   * Gets the configuration file of the application.
   *
   * @return the configuration file or empty when it is not configured
   */
  public Optional<String> getConfigFile() {
    return Optional.ofNullable(configFile);
  }

  /**
   * Gets the program arguments of the application.
   *
   * @return the unmodifiable program arguments, empty when none are configured
   */
  public List<String> getProgramArguments() {
    return programArguments;
  }

  private static Optional<Config> readSource(Path source) throws IOException {
    if (!Files.exists(source)) {
      return Optional.empty();
    }

    return Files.isDirectory(source) ? readFolder(source) : readJar(source);
  }

  private static Optional<Config> readJar(Path applicationJar) throws IOException {
    try (JarFile jar = new JarFile(applicationJar.toFile())) {
      JarEntry entry = jar.getJarEntry(CONFIGURATION);
      if (entry == null) {
        return Optional.empty();
      }

      try (Reader reader =
          new InputStreamReader(jar.getInputStream(entry), StandardCharsets.UTF_8)) {
        return Optional.of(parseConfiguration(reader));
      }
    }
  }

  private static Optional<Config> readFolder(Path classFolder) throws IOException {
    Path file = classFolder.resolve(CONFIGURATION);
    if (!Files.isRegularFile(file)) {
      return Optional.empty();
    }

    try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
      return Optional.of(parseConfiguration(reader));
    }
  }

  private static Config parseConfiguration(Reader reader) throws DeploymentException {
    try {
      return ConfigFactory.parseReader(reader).resolve();
    } catch (ConfigException invalid) {
      throw createInvalidFailure(invalid);
    }
  }

  private static DeploymentException createInvalidFailure(ConfigException invalid) {
    return new DeploymentException(Reason.INVALID_ARCHIVE,
        "BBj deployment webforj.conf is invalid. " + invalid.getMessage(), invalid);
  }

  private static Boolean readBoolean(Config configuration, String key) {
    return configuration.hasPath(key) ? configuration.getBoolean(key) : null;
  }

  private static String readAssetsPrefix(Config configuration) {
    if (!configuration.hasPath(ASSETS_DIR) || configuration.getString(ASSETS_DIR).isEmpty()) {
      return "static/";
    }

    String value = configuration.getString(ASSETS_DIR);

    return value.isBlank() ? "" : toPrefix(value);
  }

  private static String readPrefix(Config configuration, String key, String fallback) {
    if (!configuration.hasPath(key)) {
      return fallback;
    }

    String prefix = toPrefix(configuration.getString(key));

    return prefix.isEmpty() ? fallback : prefix;
  }

  private static String toPrefix(String value) {
    String prefix = value.replace('\\', '/');
    while (prefix.startsWith("/")) {
      prefix = prefix.substring(1);
    }

    if (prefix.isEmpty() || prefix.endsWith("/")) {
      return prefix;
    }

    return prefix + "/";
  }

  private static List<String> readStrings(Config configuration, String key, List<String> fallback) {
    if (!configuration.hasPath(key)) {
      return fallback;
    }

    try {
      return configuration.getStringList(key);
    } catch (ConfigException.WrongType wrongType) {
      return List.of(configuration.getString(key));
    }
  }

  private static Map<String, String> readMimeExtensions(Config configuration) {
    Map<String, String> result = new HashMap<>();
    if (configuration.hasPath(MIME_EXTENSIONS)) {
      for (Map.Entry<String, ConfigValue> entry : configuration.getConfig(MIME_EXTENSIONS)
          .entrySet()) {
        result.put(entry.getKey(), String.valueOf(entry.getValue().unwrapped()));
      }
    }

    return Map.copyOf(result);
  }
}
