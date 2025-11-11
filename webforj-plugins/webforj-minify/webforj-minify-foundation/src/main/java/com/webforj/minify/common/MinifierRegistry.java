package com.webforj.minify.common;

import java.util.Map;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry for discovering and managing asset minifier implementations.
 *
 * <p>
 * Uses Java SPI (Service Provider Interface) to automatically discover minifier implementations on
 * the classpath.
 * </p>
 *
 * <p>
 * This class is thread-safe and can be safely used in parallel processing.
 * </p>
 *
 * @author Kevin Hagel
 */
public class MinifierRegistry {
  private static final System.Logger LOGGER = System.getLogger(MinifierRegistry.class.getName());

  private final Map<String, AssetMinifier> minifiers = new ConcurrentHashMap<>();
  private BuildLogger buildLogger;

  /**
   * Sets the build logger for configuration logging.
   *
   * @param logger the build logger to use
   */
  public void setBuildLogger(BuildLogger logger) {
    this.buildLogger = logger;
  }

  /**
   * Registers a minifier for its supported file extensions.
   *
   * <p>
   * Extensions are automatically normalized (lowercased and dots removed). If multiple minifiers
   * support the same extension, the last one registered wins.
   * </p>
   *
   * @param minifier the minifier to register
   */
  public void register(AssetMinifier minifier) {
    for (String extension : minifier.getSupportedExtensions()) {
      // Normalize extension: remove leading dot and lowercase
      String normalized = normalizeExtension(extension);

      AssetMinifier previous = minifiers.put(normalized, minifier);
      if (previous != null) {
        LOGGER.log(System.Logger.Level.WARNING,
            String.format("Extension '.%s' already registered to %s, replacing with %s", normalized,
                previous.getClass().getSimpleName(), minifier.getClass().getSimpleName()));
      }
      LOGGER.log(System.Logger.Level.INFO, String.format("Registered minifier for .%s: %s",
          normalized, minifier.getClass().getSimpleName()));
    }
  }

  /**
   * Normalizes a file extension by removing leading dots and converting to lowercase.
   *
   * @param extension the extension to normalize (e.g., ".CSS" or "css")
   * @return normalized extension (e.g., "css")
   */
  private String normalizeExtension(String extension) {
    if (extension == null || extension.isEmpty()) {
      return "";
    }
    String normalized = extension.toLowerCase();
    return normalized.startsWith(".") ? normalized.substring(1) : normalized;
  }

  /**
   * Gets the minifier for a given file extension.
   *
   * <p>
   * The extension is automatically normalized before lookup.
   * </p>
   *
   * @param fileExtension the file extension (with or without dot, e.g., ".css" or "css")
   * @return an Optional containing the minifier if found
   */
  public Optional<AssetMinifier> getMinifier(String fileExtension) {
    return Optional.ofNullable(minifiers.get(normalizeExtension(fileExtension)));
  }

  /**
   * Discovers and loads all minifier implementations using Java SPI.
   *
   * <p>
   * Minifiers are loaded from META-INF/services/com.webforj.minify.common.AssetMinifier. Failed
   * registrations are logged as warnings but do not stop the loading process.
   * </p>
   *
   * @param classLoader the class loader to use for discovery
   */
  public void loadMinifiers(ClassLoader classLoader) {
    ServiceLoader<AssetMinifier> loader = ServiceLoader.load(AssetMinifier.class, classLoader);

    int loadedCount = 0;
    for (AssetMinifier minifier : loader) {
      try {
        register(minifier);
        loadedCount++;
      } catch (Exception e) {
        LOGGER.log(System.Logger.Level.WARNING, String.format("Failed to load minifier %s: %s",
            minifier.getClass().getName(), e.getMessage()));
      }
    }

    LOGGER.log(System.Logger.Level.INFO,
        String.format("Loaded %d minifier implementation(s)", loadedCount));
  }

  /**
   * Gets the number of registered minifiers.
   *
   * @return the count of registered minifiers
   */
  public int getMinifierCount() {
    return minifiers.size();
  }

  /**
   * Configures all registered minifiers with the provided configuration map.
   *
   * <p>
   * Each minifier can extract its specific configuration from the map using a well-known key. For
   * example, the Closure JS minifier looks for the "closureJs" key.
   * </p>
   *
   * @param config configuration map containing minifier-specific options
   */
  public void configureMinifiers(Map<String, Object> config) {
    if (config == null || config.isEmpty()) {
      if (buildLogger != null) {
        buildLogger.debug("No configuration provided to registry");
      }
      return;
    }

    if (buildLogger != null) {
      buildLogger
          .debug("Configuring " + minifiers.size() + " minifier(s) with keys: " + config.keySet());
    }

    for (AssetMinifier minifier : minifiers.values()) {
      try {
        minifier.configure(config);
        if (buildLogger != null) {
          buildLogger.debug("Configured: " + minifier.getClass().getSimpleName());
        }
      } catch (Exception e) {
        if (buildLogger != null) {
          buildLogger.warn(String.format("Failed to configure minifier %s: %s",
              minifier.getClass().getSimpleName(), e.getMessage()));
        }
      }
    }
  }
}
