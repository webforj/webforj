package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class WebforjConfigBuilderTest {

  private SpringConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new SpringConfigurationProperties();
  }

  @Test
  void shouldBuildConfigWithDefaults() {
    Config config = WebforjConfigBuilder.buildConfig(properties);

    assertNotNull(config);
    assertTrue(config.hasPath("webforj"));
  }

  @Test
  void shouldBuildConfigWithBasicProperties() {
    properties.setEntry("com.example.MyApp");
    properties.setDebug(true);
    properties.setComponents("https://cdn.example.com/components");
    properties.setLocale("de-DE");

    Config config = WebforjConfigBuilder.buildConfig(properties);

    assertEquals("com.example.MyApp", config.getString("webforj.entry"));
    assertTrue(config.getBoolean("webforj.debug"));
    assertEquals("https://cdn.example.com/components", config.getString("webforj.components"));
    assertEquals("de-DE", config.getString("webforj.locale"));
  }

  @Test
  void shouldBuildConfigWithStringTable() {
    Map<String, String> stringTable = new HashMap<>();
    stringTable.put("app.title", "Test Application");
    stringTable.put("app.version", "2.0.0");
    properties.setStringTable(stringTable);

    Config config = WebforjConfigBuilder.buildConfig(properties);

    assertEquals("Test Application", config.getString("webforj.stringTable.app.title"));
    assertEquals("2.0.0", config.getString("webforj.stringTable.app.version"));
  }

  @Test
  void shouldBuildConfigWithFileUpload() {
    SpringConfigurationProperties.FileUpload fileUpload =
        new SpringConfigurationProperties.FileUpload();
    fileUpload.setAccept(Arrays.asList("image/jpeg", "image/png"));
    fileUpload.setMaxSize(5242880L);
    properties.setFileUpload(fileUpload);

    Config config = WebforjConfigBuilder.buildConfig(properties);

    assertEquals(2, config.getStringList("webforj.fileUpload.accept").size());
    assertTrue(config.getStringList("webforj.fileUpload.accept").contains("image/jpeg"));
    assertEquals(5242880L, config.getLong("webforj.fileUpload.maxSize"));
  }

  @Test
  void shouldBuildConfigWithAssetsProperties() {
    properties.setQuiet(true);
    properties.setAssetsCacheControl("max-age=3600, public");
    properties.setAssetsExt(".html");
    properties.setIconsDir("icons/");

    Config config = WebforjConfigBuilder.buildConfig(properties);

    assertTrue(config.getBoolean("webforj.quiet"));
    assertEquals("max-age=3600, public", config.getString("webforj.assetsCacheControl"));
    assertEquals(".html", config.getString("webforj.assetsExt"));
    assertEquals("icons/", config.getString("webforj.iconsDir"));
  }
}
