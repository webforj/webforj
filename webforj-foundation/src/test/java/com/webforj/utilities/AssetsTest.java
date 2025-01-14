package com.webforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AssetsTest {

  @BeforeEach
  void setUp() {
    System.clearProperty("webforj.context");
  }

  @Test
  void shouldReturnFilesUrlWhenBBjServicesEnabled() {
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/files/myApp/", result);
    }
  }

  @Test
  void shouldReturnFilesUrlWithContextProperty() {
    System.setProperty("webforj.context", "/customContext");
    try (MockedStatic<Environment> environmentMock = mockStatic(Environment.class);
        MockedStatic<App> appMock = mockStatic(App.class)) {
      environmentMock.when(Environment::isRunningWithBBjServices).thenReturn(true);
      appMock.when(App::getApplicationName).thenReturn("myApp");

      String result = Assets.getWebServerFilesUrl();
      assertEquals("/customContext/files/myApp/", result);
    } finally {
      System.clearProperty("webforj.context");
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
}
