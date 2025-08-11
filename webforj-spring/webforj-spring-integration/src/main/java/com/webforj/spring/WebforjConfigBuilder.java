package com.webforj.spring;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.HashMap;
import java.util.Map;

/**
 * Builder for creating Typesafe Config objects from Spring configuration properties.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
class WebforjConfigBuilder {

  /**
   * Builds a Typesafe Config object from Spring configuration properties.
   *
   * @param properties the Spring configuration properties
   * @return a Config object with all configured values
   */
  public static Config buildConfig(SpringConfigurationProperties properties) {
    Map<String, Object> configMap = new HashMap<>();

    // Add entry point configuration
    if (properties.getEntry() != null) {
      configMap.put("webforj.entry", properties.getEntry());
    }

    // Add debug configuration - always provide a value to avoid null issues
    configMap.put("webforj.debug",
        properties.getDebug() != null ? properties.getDebug().booleanValue() : false);

    // Add components configuration
    if (properties.getComponents() != null) {
      configMap.put("webforj.components", properties.getComponents());
    }

    // Add locale configuration
    if (properties.getLocale() != null) {
      configMap.put("webforj.locale", properties.getLocale());
    }

    // Add string table configuration
    if (properties.getStringTable() != null && !properties.getStringTable().isEmpty()) {
      for (Map.Entry<String, String> entry : properties.getStringTable().entrySet()) {
        configMap.put("webforj.stringTable." + entry.getKey(), entry.getValue());
      }
    }

    // Add file upload configuration
    if (properties.getFileUpload() != null) {
      SpringConfigurationProperties.FileUpload fileUpload = properties.getFileUpload();

      if (fileUpload.getAccept() != null && !fileUpload.getAccept().isEmpty()) {
        configMap.put("webforj.fileUpload.accept", fileUpload.getAccept());
      }

      if (fileUpload.getMaxSize() != null) {
        configMap.put("webforj.fileUpload.maxSize", fileUpload.getMaxSize());
      }
    }

    // Add reload on server error configuration - provide default to avoid null
    configMap.put("webforj.reloadOnServerError",
        properties.getReloadOnServerError() != null
            ? properties.getReloadOnServerError().booleanValue()
            : false);

    // Add client heartbeat rate configuration
    if (properties.getClientHeartbeatRate() != null) {
      configMap.put("webforj.clientHeartbeatRate", properties.getClientHeartbeatRate());
    }

    // Add assets directory configuration
    if (properties.getAssetsDir() != null) {
      configMap.put("webforj.assetsDir", properties.getAssetsDir());
    }

    // Add assets cache control configuration
    if (properties.getAssetsCacheControl() != null) {
      configMap.put("webforj.assetsCacheControl", properties.getAssetsCacheControl());
    }

    // Add assets extension configuration
    if (properties.getAssetsExt() != null) {
      configMap.put("webforj.assetsExt", properties.getAssetsExt());
    }

    // Add icons directory configuration
    if (properties.getIconsDir() != null) {
      configMap.put("webforj.iconsDir", properties.getIconsDir());
    }

    // Add quiet mode configuration - provide default to avoid null
    configMap.put("webforj.quiet",
        properties.getQuiet() != null ? properties.getQuiet().booleanValue() : false);

    return ConfigFactory.parseMap(configMap);
  }
}
