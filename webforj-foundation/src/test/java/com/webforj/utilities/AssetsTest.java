package com.webforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class AssetsTest {

  @Test
  void shouldReturnPackagedFilesUrlWhenBbjServicesEnabledAndConfigured() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      createConfig(environmentMock, Map.of("webforj.assetsSource", "packaged"));
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/webforj/myApp/static/", result);
    }
  }

  @Test
  void shouldReturnConfiguredPackagedFilesUrlWhenBbjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      createConfig(environmentMock,
          Map.of("webforj.assetsSource", "packaged", "webforj.assetsDir", "assets/"));
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/webforj/myApp/assets/", result);
    }
  }

  @Test
  void shouldReturnFilesUrlWhenBBjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      createConfig(environmentMock, Map.of());
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/files/myApp/", result);
    }
  }

  @Test
  void shouldReturnPackagedIconsEndpointWhenBbjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      createConfig(environmentMock,
          Map.of("webforj.assetsSource", "packaged", "webforj.iconsDir", "brand/"));
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.resolveIconsUrl("icons://icon-144x144.png");
      assertEquals("/webforj/myApp/brand/icon-144x144.png", result);
    }
  }

  @Test
  void shouldReturnFilesUrlWithContextProperty() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/customContext");
      createConfig(environmentMock, Map.of());
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/customContext/files/myApp/", result);
    }
  }

  @Test
  void shouldPrefixFilesUrlWithConfiguredOriginOnEmbeddedPage() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      environmentMock.when(Environment::getOrigin).thenReturn("https://demo.example");
      createConfig(environmentMock, Map.of());
      appMock.when(App::getApplicationName).thenReturn("myApp");
      Page embeddedPage = mock(Page.class);
      when(embeddedPage.isEmbedded()).thenReturn(true);
      pageMock.when(Page::isPresent).thenReturn(true);
      pageMock.when(Page::getCurrent).thenReturn(embeddedPage);

      String result = Assets.getWebServerFilesUrl();
      assertEquals("https://demo.example/files/myApp/", result);
    }
  }

  @Test
  void shouldKeepRelativeFilesUrlOnDirectlyOpenedPage() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      environmentMock.when(Environment::getOrigin).thenReturn("https://demo.example");
      createConfig(environmentMock, Map.of());
      appMock.when(App::getApplicationName).thenReturn("myApp");
      pageMock.when(Page::isPresent).thenReturn(false);

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/files/myApp/", result);
    }
  }

  @Test
  void shouldPrefixIconsEndpointWithConfiguredOriginOnEmbeddedPage() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(false);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      environmentMock.when(Environment::getOrigin).thenReturn("https://demo.example");
      Environment mockEnvironment = mock(Environment.class);
      Config mockConfig = mock(Config.class);
      environmentMock.when(Environment::getCurrent).thenReturn(mockEnvironment);
      when(mockEnvironment.getConfig()).thenReturn(mockConfig);
      when(mockConfig.hasPath("webforj.iconsDir")).thenReturn(false);
      Page embeddedPage = mock(Page.class);
      when(embeddedPage.isEmbedded()).thenReturn(true);
      pageMock.when(Page::isPresent).thenReturn(true);
      pageMock.when(Page::getCurrent).thenReturn(embeddedPage);

      String result = Assets.getIconsEndpoint();
      assertEquals("https://demo.example/icons/", result);
    }
  }

  @Test
  void shouldIdentifyWebServerUrlCorrectly() {
    assertTrue(Assets.isWebServerUrl("webserver://static/css/foo.css"));
    assertTrue(Assets.isWebServerUrl("ws://static/css/foo.css"));
    assertFalse(Assets.isWebServerUrl("http://example.com/static/css/foo.css"));
  }

  @Test
  void shouldThrowExceptionForNonWebServerUrlInResolveWebServerUrl() {
    assertThrows(IllegalArgumentException.class,
        () -> Assets.resolveWebServerUrl("http://example.com/static/css/foo.css"));
  }

  @Test
  void shouldIdentifyContextUrlCorrectly() {
    assertTrue(Assets.isContextUrl("context://local/path"));
    assertFalse(Assets.isContextUrl("http://example.com/path"));
  }

  @Test
  void shouldResolveContextUrlForValidInput() {
    String result = Assets.resolveContextUrl("context://local/path");
    assertEquals("local/path", result);
  }

  @Test
  void shouldThrowExceptionForNonContextUrlInResolveContextUrl() {
    assertThrows(IllegalArgumentException.class,
        () -> Assets.resolveContextUrl("http://example.com/path"));
  }

  @Test
  void shouldReturnFileNameForValidPath() {
    assertEquals("foo.css", Assets.getFileName("ws://static/css/foo.css"));
    assertEquals("foo.css", Assets.getFileName("/static/css/foo.css"));
    assertEquals("foo.css", Assets.getFileName("static/css/foo.css"));
    assertEquals("foo.css", Assets.getFileName("ws://foo.css"));
    assertEquals("foo.css", Assets.getFileName("foo.css"));
    assertEquals("foo", Assets.getFileName("ws://foo"));
    assertEquals("", Assets.getFileName(""));
    assertEquals("", Assets.getFileName(null));
  }

  @Test
  void shouldReturnFileExtensionForValidFileName() {
    assertEquals(".css", Assets.getFileExtension("foo.css"));
    assertEquals(".css", Assets.getFileExtension("/static/css/foo.css"));
    assertEquals(".css", Assets.getFileExtension("static/css/foo.css"));
    assertEquals("", Assets.getFileExtension("foo"));
    assertEquals("", Assets.getFileExtension(""));
    assertEquals("", Assets.getFileExtension(null));
  }

  @Test
  void shouldIdentifyIconsUrlCorrectly() {
    assertTrue(Assets.isIconsUrl("icons://icon/path"));
    assertFalse(Assets.isIconsUrl("http://example.com/icon/path"));
  }

  @Test
  void shouldResolveIconsUrlForValidInput() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(false);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      Environment mockEnvironment = mock(Environment.class);
      Config mockConfig = mock(Config.class);
      environmentMock.when(Environment::getCurrent).thenReturn(mockEnvironment);
      when(mockEnvironment.getConfig()).thenReturn(mockConfig);
      when(mockConfig.hasPath("webforj.iconsDir")).thenReturn(false);

      String result = Assets.resolveIconsUrl("icons://icon/path");
      assertEquals("/icons/icon/path", result);
    }
  }

  @Test
  void shouldThrowExceptionForNonIconsUrlInResolveIconsUrl() {
    assertThrows(IllegalArgumentException.class,
        () -> Assets.resolveIconsUrl("http://example.com/icon/path"));
  }

  @Test
  void shouldResolveIconsUrlWhenBbjServicesEnabledAndConfigured() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      createConfig(environmentMock, Map.of("webforj.assetsSource", "packaged"));
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.resolveIconsUrl("icons://icon/path");
      assertEquals("/webforj/myApp/icons/icon/path", result);
    }
  }

  @Test
  void shouldThrowExceptionForIconsUrlWhenBbjServicesEnabledWithoutPackagedAssets() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      createConfig(environmentMock, Map.of());

      assertThrows(WebforjRuntimeException.class,
          () -> Assets.resolveIconsUrl("icons://icon/path"));
    }
  }

  @ParameterizedTest
  @MethodSource("getAssetsSourceCases")
  void shouldTellWhetherAssetsArePackagedOnBbj(Map<String, String> values, boolean expected) {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class)) {
      createConfig(environmentMock, values);

      assertEquals(expected, Assets.isPackagedOnBBj());
    }
  }

  private static Stream<Arguments> getAssetsSourceCases() {
    return Stream.of(Arguments.of(Map.of("webforj.assetsSource", "packaged"), true),
        Arguments.of(Map.of("webforj.assetsSource", "files"), false),
        Arguments.of(Map.of(), false));
  }

  private static Config createConfig(MockedStatic<Environment> environmentMock,
      Map<String, String> values) {
    Environment environment = mock(Environment.class);
    Config config = mock(Config.class);
    environmentMock.when(Environment::getCurrent).thenReturn(environment);
    when(environment.getConfig()).thenReturn(config);
    values.forEach((key, value) -> {
      when(config.hasPath(key)).thenReturn(true);
      when(config.getString(key)).thenReturn(value);
    });

    return config;
  }
}
