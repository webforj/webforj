package com.webforj.spring;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Builder for creating Typesafe Config objects from Spring configuration properties.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
class WebforjConfigBuilder {

  private WebforjConfigBuilder() {
    // prevent instantiation
  }

  /**
   * Builds a Typesafe Config object from Spring configuration properties.
   *
   * @param properties the Spring configuration properties
   * @return a Config object with all configured values
   */
  public static Config buildConfig(SpringConfigurationProperties properties) {
    ConfigMapBuilder builder = new ConfigMapBuilder();

    // Basic configurations
    builder.add("webforj.entry", properties::getEntry)
        .add("webforj.debug", properties::getDebug, false)
        .add("webforj.components", properties::getComponents)
        .add("webforj.locale", properties::getLocale)
        .add("webforj.quiet", properties::getQuiet, false)
        .add("webforj.reloadOnServerError", properties::getReloadOnServerError, false);

    // Client configuration
    builder.add("webforj.clientHeartbeatRate", properties::getClientHeartbeatRate)
        .add("webforj.sessionTimeout", properties::getSessionTimeout, 60);

    // Assets configurations
    builder.add("webforj.assetsDir", properties::getAssetsDir)
        .add("webforj.assetsCacheControl", properties::getAssetsCacheControl)
        .add("webforj.assetsExt", properties::getAssetsExt)
        .add("webforj.assetsIndex", properties::getAssetsIndex)
        .add("webforj.iconsDir", properties::getIconsDir);

    // String table configuration
    builder.addMap("webforj.stringTable", properties::getStringTable);

    // File upload configuration
    builder.addNested(properties::getFileUpload, fileUpload -> {
      builder.addList("webforj.fileUpload.accept", fileUpload::getAccept)
          .add("webforj.fileUpload.maxSize", fileUpload::getMaxSize);
    });

    // License configuration
    builder.addNested(properties::getLicense, license -> {
      builder.add("webforj.license.cfg", license::getCfg).add("webforj.license.startupTimeout",
          license::getStartupTimeout);
    });

    // Servlet configurations
    builder.add("webforj.servlets", () -> {
      if (properties.getServlets() == null || properties.getServlets().isEmpty()) {
        return null;
      }

      List<Map<String, Object>> configs = new ArrayList<>();
      for (SpringConfigurationProperties.ServletConfig servlet : properties.getServlets()) {
        Map<String, Object> config = new HashMap<>();
        if (servlet.getClassName() != null) {
          config.put("class", servlet.getClassName());
        }

        if (servlet.getName() != null) {
          config.put("name", servlet.getName());
        }

        if (servlet.getConfig() != null && !servlet.getConfig().isEmpty()) {
          config.put("config", servlet.getConfig());
        }

        configs.add(config);
      }

      return configs;
    });

    return ConfigFactory.parseMap(builder.build());
  }


  /**
   * Internal builder class for creating configuration maps.
   */
  private static class ConfigMapBuilder {
    private final Map<String, Object> configMap = new HashMap<>();

    <T> ConfigMapBuilder add(String key, Supplier<T> valueSupplier) {
      T value = valueSupplier.get();
      if (value != null) {
        configMap.put(key, value);
      }
      return this;
    }

    <T> ConfigMapBuilder add(String key, Supplier<T> valueSupplier, T defaultValue) {
      T value = valueSupplier.get();
      configMap.put(key, value != null ? value : defaultValue);
      return this;
    }

    ConfigMapBuilder addList(String key, Supplier<List<?>> valueSupplier) {
      List<?> value = valueSupplier.get();
      if (value != null && !value.isEmpty()) {
        configMap.put(key, value);
      }
      return this;
    }

    ConfigMapBuilder addMap(String prefix, Supplier<Map<String, String>> valueSupplier) {
      Map<String, String> map = valueSupplier.get();
      if (map != null && !map.isEmpty()) {
        for (Map.Entry<String, String> entry : map.entrySet()) {
          configMap.put(prefix + "." + entry.getKey(), entry.getValue());
        }
      }
      return this;
    }

    <T> ConfigMapBuilder addNested(Supplier<T> nestedSupplier, Consumer<T> nestedProcessor) {
      T nested = nestedSupplier.get();
      if (nested != null) {
        nestedProcessor.accept(nested);
      }
      return this;
    }

    Map<String, Object> build() {
      return configMap;
    }
  }
}
