package com.webforj.minify.common;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceLoader;
import java.util.logging.Logger;

/**
 * Registry for discovering and managing asset minifier implementations.
 *
 * Uses Java SPI (Service Provider Interface) to automatically discover
 * minifier implementations on the classpath.
 */
public class MinifierRegistry {
  private static final Logger LOGGER = Logger.getLogger(MinifierRegistry.class.getName());

  private final Map<String, AssetMinifier> minifiers = new HashMap<>();

  /**
   * Registers a minifier for its supported file extensions.
   *
   * @param minifier the minifier to register
   */
  public void register(AssetMinifier minifier) {
    for (String extension : minifier.getSupportedExtensions()) {
      if (minifiers.containsKey(extension)) {
        LOGGER.warning(String.format(
          "Extension '%s' already has a registered minifier. Replacing with %s",
          extension,
          minifier.getClass().getName()
        ));
      }
      minifiers.put(extension, minifier);
      LOGGER.info(String.format(
        "Registered %s minifier: %s",
        extension.toUpperCase(),
        minifier.getClass().getName()
      ));
    }
  }

  /**
   * Gets the minifier for a given file extension.
   *
   * @param fileExtension the file extension (without dot)
   * @return an Optional containing the minifier if found
   */
  public Optional<AssetMinifier> getMinifier(String fileExtension) {
    return Optional.ofNullable(minifiers.get(fileExtension));
  }

  /**
   * Discovers and loads all minifier implementations using Java SPI.
   *
   * @param classLoader the class loader to use for discovery
   */
  public void loadMinifiers(ClassLoader classLoader) {
    ServiceLoader<AssetMinifier> loader = ServiceLoader.load(AssetMinifier.class, classLoader);

    for (AssetMinifier minifier : loader) {
      try {
        register(minifier);
      } catch (Exception e) {
        LOGGER.warning(String.format(
          "Failed to load minifier %s: %s",
          minifier.getClass().getName(),
          e.getMessage()
        ));
      }
    }
  }

  /**
   * Gets the number of registered minifiers.
   *
   * @return the count of registered minifiers
   */
  public int getMinifierCount() {
    return minifiers.size();
  }
}
