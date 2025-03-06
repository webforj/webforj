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
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AssetsTest {

  @Test
  void shouldReturnFilesUrlWhenBBjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/");
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/files/myApp/", result);
    }
  }

  @Test
  void shouldReturnFilesUrlWithContextProperty() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      environmentMock.when(Environment::getContextPath).thenReturn("/customContext");
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/customContext/files/myApp/", result);
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
  void shouldThrowExceptionForIconsUrlWhenBBjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);

      assertThrows(WebforjRuntimeException.class,
          () -> Assets.resolveIconsUrl("icons://icon/path"));
    }
  }
}
