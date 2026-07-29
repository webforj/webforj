package com.webforj.devtools.craftforj.appinfo;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.appinfo.model.AppInfo;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppInfoCollectorTest {

  @Test
  @DisplayName("Should collect application and runtime information")
  void shouldCollectApplicationAndRuntimeInformation() {
    try (MockedStatic<App> appMock = mockStatic(App.class);
        MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      appMock.when(App::getApplicationName).thenReturn("Demo App");
      envMock.when(Environment::getContextPath).thenReturn("/demo");
      envMock.when(Environment::isRunningWithBBjServices).thenReturn(false);

      AppInfoCollector collector =
          new AppInfoCollector("com.example.DemoApp", Path.of("/projects/demo"));
      AppInfo info = collector.collect();

      assertEquals("Demo App", info.getAppName());
      assertEquals("com.example.DemoApp", info.getAppClass());
      assertEquals("/demo", info.getContextPath());
      assertEquals(Path.of("/projects/demo").toString(), info.getProjectRoot());
      assertFalse(info.isBbjServices());
      assertEquals(System.getProperty("java.version"), info.getJavaVersion());
      assertEquals(System.getProperty("java.vendor"), info.getJavaVendor());
      assertEquals(System.getProperty("java.vm.name"), info.getJavaVm());
      assertEquals(System.getProperty("os.name"), info.getOsName());
      assertEquals(System.getProperty("os.version"), info.getOsVersion());
      assertEquals(System.getProperty("os.arch"), info.getOsArch());
      assertTrue(info.getStartedAt() > 0);
    }
  }

  @Test
  @DisplayName("Should detect framework versions")
  void shouldDetectFrameworkVersions() {
    try (MockedStatic<App> appMock = mockStatic(App.class);
        MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      envMock.when(Environment::isRunningWithBBjServices).thenReturn(true);

      AppInfoCollector collector = new AppInfoCollector("com.example.DemoApp", null);
      AppInfo info = collector.collect();

      assertNotNull(info.getWebforjVersion());
      assertTrue(info.isBbjServices());
    }
  }

  @Test
  @DisplayName("Should leave the project root null when not provided")
  void shouldLeaveProjectRootNullWhenNotProvided() {
    try (MockedStatic<App> appMock = mockStatic(App.class);
        MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      AppInfoCollector collector = new AppInfoCollector("com.example.DemoApp", null);
      AppInfo info = collector.collect();

      assertNull(info.getProjectRoot());
    }
  }
}
