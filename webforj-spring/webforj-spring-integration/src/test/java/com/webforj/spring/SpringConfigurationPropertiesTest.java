package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SpringConfigurationPropertiesTest {

  private SpringConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new SpringConfigurationProperties();
  }

  @ParameterizedTest
  @ValueSource(strings = {"/", "/*", "/webforj/*", "/app/*", "/api/webforj/*"})
  void shouldHandleVariousServletMappingPatterns(String mapping) {
    properties.setServletMapping(mapping);
    assertEquals(mapping, properties.getServletMapping());
  }

  @Test
  void shouldHandleEntryProperty() {
    String entry = "com.example.MyApp";
    properties.setEntry(entry);
    assertEquals(entry, properties.getEntry());
  }

  @Test
  void shouldHandleDebugProperty() {
    properties.setDebug(true);
    assertEquals(Boolean.TRUE, properties.getDebug());

    properties.setDebug(false);
    assertEquals(Boolean.FALSE, properties.getDebug());
  }

  @Test
  void shouldHandleComponentsProperty() {
    String components = "https://cdn.example.com/webforj-components";
    properties.setComponents(components);
    assertEquals(components, properties.getComponents());
  }

  @Test
  void shouldHandleLocaleProperty() {
    String locale = "en-US";
    properties.setLocale(locale);
    assertEquals(locale, properties.getLocale());
  }

  @Test
  void shouldHandleStringTableProperty() {
    Map<String, String> stringTable = new HashMap<>();
    stringTable.put("app.title", "My Application");
    stringTable.put("app.version", "1.0.0");

    properties.setStringTable(stringTable);
    assertEquals(stringTable, properties.getStringTable());
    assertEquals("My Application", properties.getStringTable().get("app.title"));
  }

  @Test
  void shouldInitializeEmptyStringTable() {
    assertNotNull(properties.getStringTable());
    assertTrue(properties.getStringTable().isEmpty());
  }

  @Test
  void shouldHandleFileUploadAcceptProperty() {
    SpringConfigurationProperties.FileUpload fileUpload =
        new SpringConfigurationProperties.FileUpload();
    fileUpload.setAccept(Arrays.asList("image/*", "application/pdf", "*.txt"));

    properties.setFileUpload(fileUpload);
    assertNotNull(properties.getFileUpload());
    assertEquals(3, properties.getFileUpload().getAccept().size());
    assertTrue(properties.getFileUpload().getAccept().contains("image/*"));
  }

  @Test
  void shouldHandleFileUploadMaxSizeProperty() {
    SpringConfigurationProperties.FileUpload fileUpload =
        new SpringConfigurationProperties.FileUpload();
    fileUpload.setMaxSize(10485760L); // 10MB

    properties.setFileUpload(fileUpload);
    assertNotNull(properties.getFileUpload());
    assertEquals(10485760L, properties.getFileUpload().getMaxSize());
  }

  @Test
  void shouldInitializeFileUploadWithDefaults() {
    assertNotNull(properties.getFileUpload());
    assertNotNull(properties.getFileUpload().getAccept());
    assertTrue(properties.getFileUpload().getAccept().isEmpty());
  }

  @Test
  void shouldHandleReloadOnServerErrorProperty() {
    properties.setReloadOnServerError(true);
    assertEquals(Boolean.TRUE, properties.getReloadOnServerError());

    properties.setReloadOnServerError(false);
    assertEquals(Boolean.FALSE, properties.getReloadOnServerError());
  }

  @Test
  void shouldHandleClientHeartbeatRateProperty() {
    String heartbeatRate = "8s";
    properties.setClientHeartbeatRate(heartbeatRate);
    assertEquals(heartbeatRate, properties.getClientHeartbeatRate());
  }

  @Test
  void shouldHandleAssetsDirProperty() {
    String assetsDir = "static-files";
    properties.setAssetsDir(assetsDir);
    assertEquals(assetsDir, properties.getAssetsDir());
  }

  @Test
  void shouldHandleQuietProperty() {
    properties.setQuiet(true);
    assertEquals(Boolean.TRUE, properties.getQuiet());

    properties.setQuiet(false);
    assertEquals(Boolean.FALSE, properties.getQuiet());
  }

  @Test
  void shouldHandleAssetsCacheControlProperty() {
    String cacheControl = "max-age=3600, public";
    properties.setAssetsCacheControl(cacheControl);
    assertEquals(cacheControl, properties.getAssetsCacheControl());
  }

  @Test
  void shouldHandleAssetsExtProperty() {
    String assetsExt = ".html";
    properties.setAssetsExt(assetsExt);
    assertEquals(assetsExt, properties.getAssetsExt());
  }

  @Test
  void shouldHandleIconsDirProperty() {
    String iconsDir = "icons/";
    properties.setIconsDir(iconsDir);
    assertEquals(iconsDir, properties.getIconsDir());
  }


  @Test
  void shouldHandleServletConfigProperty() {
    SpringConfigurationProperties.ServletConfig servletConfig =
        new SpringConfigurationProperties.ServletConfig();
    servletConfig.setClassName("com.example.MyServlet");
    servletConfig.setName("myServlet");

    Map<String, String> config = new HashMap<>();
    config.put("param1", "value1");
    config.put("param2", "value2");
    servletConfig.setConfig(config);

    properties.setServlets(Arrays.asList(servletConfig));

    assertNotNull(properties.getServlets());
    assertEquals(1, properties.getServlets().size());
    assertEquals("com.example.MyServlet", properties.getServlets().get(0).getClassName());
    assertEquals("myServlet", properties.getServlets().get(0).getName());
    assertEquals(2, properties.getServlets().get(0).getConfig().size());
    assertEquals("value1", properties.getServlets().get(0).getConfig().get("param1"));
  }

  @Test
  void shouldInitializeEmptyServletsList() {
    assertNotNull(properties.getServlets());
    assertTrue(properties.getServlets().isEmpty());
  }
}
