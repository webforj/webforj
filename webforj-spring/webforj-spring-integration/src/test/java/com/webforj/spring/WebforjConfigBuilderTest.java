package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class WebforjConfigBuilderTest {

  private SpringConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new SpringConfigurationProperties();
  }

  @Nested
  class BasicProperties {

    @Test
    void shouldSetEntry() {
      properties.setEntry("com.example.App");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("com.example.App", config.getString("webforj.entry"));
    }

    @Test
    void shouldHandleNullEntry() {
      properties.setEntry(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.entry"));
    }

    @Test
    void shouldSetDebugWithDefault() {
      // Test null (should use default false)
      Config config1 = WebforjConfigBuilder.buildConfig(properties);
      assertFalse(config1.getBoolean("webforj.debug"));

      // Test explicit true
      properties.setDebug(true);
      Config config2 = WebforjConfigBuilder.buildConfig(properties);
      assertTrue(config2.getBoolean("webforj.debug"));

      // Test explicit false
      properties.setDebug(false);
      Config config3 = WebforjConfigBuilder.buildConfig(properties);
      assertFalse(config3.getBoolean("webforj.debug"));
    }

    @Test
    void shouldSetComponents() {
      properties.setComponents("https://cdn.example.com/components");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("https://cdn.example.com/components", config.getString("webforj.components"));
    }

    @Test
    void shouldSetLocale() {
      properties.setLocale("en-US");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("en-US", config.getString("webforj.locale"));
    }

    @Test
    void shouldSetQuietWithDefault() {
      // Test null (should use default false)
      Config config1 = WebforjConfigBuilder.buildConfig(properties);
      assertFalse(config1.getBoolean("webforj.quiet"));

      // Test explicit true
      properties.setQuiet(true);
      Config config2 = WebforjConfigBuilder.buildConfig(properties);
      assertTrue(config2.getBoolean("webforj.quiet"));
    }

    @Test
    void shouldSetReloadOnServerErrorWithDefault() {
      // Test null (should use default false)
      Config config1 = WebforjConfigBuilder.buildConfig(properties);
      assertFalse(config1.getBoolean("webforj.reloadOnServerError"));

      // Test explicit true
      properties.setReloadOnServerError(true);
      Config config2 = WebforjConfigBuilder.buildConfig(properties);
      assertTrue(config2.getBoolean("webforj.reloadOnServerError"));
    }
  }

  @Nested
  class ClientConfiguration {

    @Test
    void shouldSetClientHeartbeatRate() {
      properties.setClientHeartbeatRate("30s");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("30s", config.getString("webforj.clientHeartbeatRate"));
    }

    @Test
    void shouldHandleNullClientHeartbeatRate() {
      properties.setClientHeartbeatRate(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.clientHeartbeatRate"));
    }
  }

  @Nested
  class AssetsConfiguration {

    @Test
    void shouldSetAssetsDir() {
      properties.setAssetsDir("static");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("static", config.getString("webforj.assetsDir"));
    }

    @Test
    void shouldSetAssetsCacheControl() {
      properties.setAssetsCacheControl("max-age=3600");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("max-age=3600", config.getString("webforj.assetsCacheControl"));
    }

    @Test
    void shouldSetAssetsExt() {
      properties.setAssetsExt(".html");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals(".html", config.getString("webforj.assetsExt"));
    }

    @Test
    void shouldSetAssetsIndex() {
      properties.setAssetsIndex("index.html");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("index.html", config.getString("webforj.assetsIndex"));
    }

    @Test
    void shouldSetIconsDir() {
      properties.setIconsDir("icons");
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("icons", config.getString("webforj.iconsDir"));
    }

    @Test
    void shouldHandleNullAssetsProperties() {
      // All null
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.assetsDir"));
      assertFalse(config.hasPath("webforj.assetsCacheControl"));
      assertFalse(config.hasPath("webforj.assetsExt"));
      assertFalse(config.hasPath("webforj.assetsIndex"));
      assertFalse(config.hasPath("webforj.iconsDir"));
    }
  }

  @Nested
  class StringTableConfiguration {

    @Test
    void shouldSetStringTable() {
      Map<String, String> stringTable = new HashMap<>();
      stringTable.put("app.name", "MyApp");
      stringTable.put("app.version", "1.0.0");
      stringTable.put("welcome.message", "Hello World");
      properties.setStringTable(stringTable);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("MyApp", config.getString("webforj.stringTable.app.name"));
      assertEquals("1.0.0", config.getString("webforj.stringTable.app.version"));
      assertEquals("Hello World", config.getString("webforj.stringTable.welcome.message"));
    }

    @Test
    void shouldHandleEmptyStringTable() {
      properties.setStringTable(new HashMap<>());
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.stringTable"));
    }

    @Test
    void shouldHandleNullStringTable() {
      properties.setStringTable(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.stringTable"));
    }
  }

  @Nested
  class FileUploadConfiguration {

    @Test
    void shouldSetFileUploadAccept() {
      SpringConfigurationProperties.FileUpload fileUpload =
          new SpringConfigurationProperties.FileUpload();
      fileUpload.setAccept(Arrays.asList("image/*", "application/pdf", ".txt"));
      properties.setFileUpload(fileUpload);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      List<String> accept = config.getStringList("webforj.fileUpload.accept");
      assertEquals(3, accept.size());
      assertTrue(accept.contains("image/*"));
      assertTrue(accept.contains("application/pdf"));
      assertTrue(accept.contains(".txt"));
    }

    @Test
    void shouldSetFileUploadMaxSize() {
      SpringConfigurationProperties.FileUpload fileUpload =
          new SpringConfigurationProperties.FileUpload();
      fileUpload.setMaxSize(10485760L);
      properties.setFileUpload(fileUpload);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals(10485760L, config.getLong("webforj.fileUpload.maxSize"));
    }

    @Test
    void shouldHandleEmptyFileUploadAccept() {
      SpringConfigurationProperties.FileUpload fileUpload =
          new SpringConfigurationProperties.FileUpload();
      fileUpload.setAccept(Arrays.asList());
      properties.setFileUpload(fileUpload);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.fileUpload.accept"));
    }

    @Test
    void shouldHandleNullFileUpload() {
      properties.setFileUpload(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.fileUpload"));
    }

    @Test
    void shouldHandleFileUploadWithNullValues() {
      SpringConfigurationProperties.FileUpload fileUpload =
          new SpringConfigurationProperties.FileUpload();
      fileUpload.setAccept(null);
      fileUpload.setMaxSize(null);
      properties.setFileUpload(fileUpload);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.fileUpload.accept"));
      assertFalse(config.hasPath("webforj.fileUpload.maxSize"));
    }
  }

  @Nested
  class LicenseConfiguration {

    @Test
    void shouldSetLicenseCfg() {
      SpringConfigurationProperties.License license = new SpringConfigurationProperties.License();
      license.setCfg("/path/to/license.cfg");
      properties.setLicense(license);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("/path/to/license.cfg", config.getString("webforj.license.cfg"));
    }

    @Test
    void shouldSetLicenseStartupTimeout() {
      SpringConfigurationProperties.License license = new SpringConfigurationProperties.License();
      license.setStartupTimeout(60);
      properties.setLicense(license);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals(60, config.getInt("webforj.license.startupTimeout"));
    }

    @Test
    void shouldSetBothLicenseProperties() {
      SpringConfigurationProperties.License license = new SpringConfigurationProperties.License();
      license.setCfg("/licenses/app.cfg");
      license.setStartupTimeout(30);
      properties.setLicense(license);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals("/licenses/app.cfg", config.getString("webforj.license.cfg"));
      assertEquals(30, config.getInt("webforj.license.startupTimeout"));
    }

    @Test
    void shouldHandleNullLicense() {
      properties.setLicense(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.license.cfg"));
      assertFalse(config.hasPath("webforj.license.startupTimeout"));
    }

    @Test
    void shouldHandleLicenseWithNullValues() {
      SpringConfigurationProperties.License license = new SpringConfigurationProperties.License();
      license.setCfg(null);
      license.setStartupTimeout(null);
      properties.setLicense(license);

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.license.cfg"));
      assertFalse(config.hasPath("webforj.license.startupTimeout"));
    }
  }

  @Nested
  class ServletConfiguration {

    @Test
    void shouldSetSingleServlet() {
      SpringConfigurationProperties.ServletConfig servlet =
          new SpringConfigurationProperties.ServletConfig();
      servlet.setClassName("org.springframework.web.servlet.DispatcherServlet");
      servlet.setName("dispatcher");

      Map<String, String> servletConfig = new HashMap<>();
      servletConfig.put("contextClass",
          "org.springframework.web.context.support.AnnotationConfigWebApplicationContext");
      servletConfig.put("contextConfigLocation", "com.example.config.AppConfig");
      servlet.setConfig(servletConfig);

      properties.setServlets(Arrays.asList(servlet));

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertTrue(config.hasPath("webforj.servlets"));
      assertEquals(1, config.getConfigList("webforj.servlets").size());

      Config servletConf = config.getConfigList("webforj.servlets").get(0);
      assertEquals("org.springframework.web.servlet.DispatcherServlet",
          servletConf.getString("class"));
      assertEquals("dispatcher", servletConf.getString("name"));
      assertEquals("org.springframework.web.context.support.AnnotationConfigWebApplicationContext",
          servletConf.getConfig("config").getString("contextClass"));
      assertEquals("com.example.config.AppConfig",
          servletConf.getConfig("config").getString("contextConfigLocation"));
    }

    @Test
    void shouldSetMultipleServlets() {
      SpringConfigurationProperties.ServletConfig servlet1 =
          new SpringConfigurationProperties.ServletConfig();
      servlet1.setClassName("com.example.MyServlet");
      servlet1.setName("myServlet");

      SpringConfigurationProperties.ServletConfig servlet2 =
          new SpringConfigurationProperties.ServletConfig();
      servlet2.setClassName("com.example.AnotherServlet");
      servlet2.setName("anotherServlet");

      properties.setServlets(Arrays.asList(servlet1, servlet2));

      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertEquals(2, config.getConfigList("webforj.servlets").size());

      Config firstServlet = config.getConfigList("webforj.servlets").get(0);
      assertEquals("com.example.MyServlet", firstServlet.getString("class"));
      assertEquals("myServlet", firstServlet.getString("name"));

      Config secondServlet = config.getConfigList("webforj.servlets").get(1);
      assertEquals("com.example.AnotherServlet", secondServlet.getString("class"));
      assertEquals("anotherServlet", secondServlet.getString("name"));
    }

    @Test
    void shouldHandleServletWithNullName() {
      SpringConfigurationProperties.ServletConfig servlet =
          new SpringConfigurationProperties.ServletConfig();
      servlet.setClassName("com.example.MyServlet");
      servlet.setName(null);

      properties.setServlets(Arrays.asList(servlet));

      Config config = WebforjConfigBuilder.buildConfig(properties);

      Config servletConf = config.getConfigList("webforj.servlets").get(0);
      assertEquals("com.example.MyServlet", servletConf.getString("class"));
      assertFalse(servletConf.hasPath("name"));
    }

    @Test
    void shouldHandleServletWithEmptyConfig() {
      SpringConfigurationProperties.ServletConfig servlet =
          new SpringConfigurationProperties.ServletConfig();
      servlet.setClassName("com.example.MyServlet");
      servlet.setConfig(new HashMap<>());

      properties.setServlets(Arrays.asList(servlet));

      Config config = WebforjConfigBuilder.buildConfig(properties);

      Config servletConf = config.getConfigList("webforj.servlets").get(0);
      assertFalse(servletConf.hasPath("config"));
    }

    @Test
    void shouldHandleEmptyServletsList() {
      properties.setServlets(Arrays.asList());
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.servlets"));
    }

    @Test
    void shouldHandleNullServletsList() {
      properties.setServlets(null);
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertFalse(config.hasPath("webforj.servlets"));
    }
  }

  @Nested
  class CompleteConfiguration {

    @Test
    void shouldBuildCompleteConfiguration() {
      // Basic properties
      properties.setEntry("com.example.MainApp");
      properties.setDebug(true);
      properties.setComponents("https://cdn.example.com/webforj");
      properties.setLocale("de-DE");
      properties.setQuiet(false);
      properties.setReloadOnServerError(true);

      // Client
      properties.setClientHeartbeatRate("10s");

      // Assets
      properties.setAssetsDir("assets");
      properties.setAssetsCacheControl("no-cache");
      properties.setAssetsExt(".htm");
      properties.setAssetsIndex("main.html");
      properties.setIconsDir("img/icons");

      // String table
      Map<String, String> stringTable = new HashMap<>();
      stringTable.put("title", "Test App");
      properties.setStringTable(stringTable);

      // File upload
      SpringConfigurationProperties.FileUpload fileUpload =
          new SpringConfigurationProperties.FileUpload();
      fileUpload.setAccept(Arrays.asList("*"));
      fileUpload.setMaxSize(1000000L);
      properties.setFileUpload(fileUpload);

      // License
      SpringConfigurationProperties.License license = new SpringConfigurationProperties.License();
      license.setCfg("/opt/license");
      license.setStartupTimeout(120);
      properties.setLicense(license);

      // Servlets
      SpringConfigurationProperties.ServletConfig servlet =
          new SpringConfigurationProperties.ServletConfig();
      servlet.setClassName("com.example.TestServlet");
      servlet.setName("test");
      properties.setServlets(Arrays.asList(servlet));

      Config config = WebforjConfigBuilder.buildConfig(properties);

      // Verify everything is set
      assertEquals("com.example.MainApp", config.getString("webforj.entry"));
      assertTrue(config.getBoolean("webforj.debug"));
      assertEquals("https://cdn.example.com/webforj", config.getString("webforj.components"));
      assertEquals("de-DE", config.getString("webforj.locale"));
      assertFalse(config.getBoolean("webforj.quiet"));
      assertTrue(config.getBoolean("webforj.reloadOnServerError"));
      assertEquals("10s", config.getString("webforj.clientHeartbeatRate"));
      assertEquals("assets", config.getString("webforj.assetsDir"));
      assertEquals("no-cache", config.getString("webforj.assetsCacheControl"));
      assertEquals(".htm", config.getString("webforj.assetsExt"));
      assertEquals("main.html", config.getString("webforj.assetsIndex"));
      assertEquals("img/icons", config.getString("webforj.iconsDir"));
      assertEquals("Test App", config.getString("webforj.stringTable.title"));
      assertEquals(Arrays.asList("*"), config.getStringList("webforj.fileUpload.accept"));
      assertEquals(1000000L, config.getLong("webforj.fileUpload.maxSize"));
      assertEquals("/opt/license", config.getString("webforj.license.cfg"));
      assertEquals(120, config.getInt("webforj.license.startupTimeout"));
      assertEquals(1, config.getConfigList("webforj.servlets").size());
      assertEquals("com.example.TestServlet",
          config.getConfigList("webforj.servlets").get(0).getString("class"));
    }

    @Test
    void shouldBuildMinimalConfiguration() {
      // Build with all nulls/defaults
      Config config = WebforjConfigBuilder.buildConfig(properties);

      assertNotNull(config);
      // Only defaults should be set
      assertFalse(config.getBoolean("webforj.debug"));
      assertFalse(config.getBoolean("webforj.quiet"));
      assertFalse(config.getBoolean("webforj.reloadOnServerError"));

      // Everything else should be missing
      assertFalse(config.hasPath("webforj.entry"));
      assertFalse(config.hasPath("webforj.components"));
      assertFalse(config.hasPath("webforj.locale"));
      assertFalse(config.hasPath("webforj.servlets"));
    }
  }
}
