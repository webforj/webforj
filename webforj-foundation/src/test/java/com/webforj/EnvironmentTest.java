package com.webforj;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjInterpreter;
import com.basis.bbj.proxies.BBjSysGui;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.error.ErrorHandler;
import com.webforj.error.GlobalErrorHandler;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class EnvironmentTest {

  @Nested
  class ErrorHandling {

    ErrorHandler specificHandler;
    ErrorHandler specificGlobalHandler;
    ErrorHandler globalHandler;

    @BeforeEach
    void setUp() {
      specificHandler = mock(IllegalArgumentExceptionErrorHandler.class);
      specificGlobalHandler = mock(WebforjGlobalErrorHandler.class);
      globalHandler = mock(GlobalErrorHandler.class);
    }

    @Test
    void shouldHandleErrorWithSpecificHandler() {
      try (MockedStatic<Environment> mockedEnvStatic =
          mockStatic(Environment.class, CALLS_REAL_METHODS)) {

        mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

        ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
        when(mockLoader.iterator()).thenReturn(List.of(specificHandler).iterator());

        Exception ex = new IllegalArgumentException();
        Environment.handleError(ex, 1, mockLoader);

        verify(specificHandler).onError(ex, true);
      }
    }

    @Test
    void shouldHandleErrorWithGlobalHandler() {
      try (MockedStatic<Environment> mockedEnvStatic =
          mockStatic(Environment.class, CALLS_REAL_METHODS)) {

        mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

        ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
        when(mockLoader.iterator()).thenReturn((new ArrayList<ErrorHandler>()).iterator());

        Exception ex = new IllegalArgumentException();
        Environment.handleError(ex, 1, mockLoader);

        verify(globalHandler).onError(ex, true);
      }
    }

    @Test
    void shouldHandleErrorWithSpecificGlobalHandler() {
      try (MockedStatic<Environment> mockedEnvStatic =
          mockStatic(Environment.class, CALLS_REAL_METHODS)) {

        mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

        ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
        when(mockLoader.iterator()).thenReturn(List.of(specificGlobalHandler).iterator());

        Exception ex = new IllegalArgumentException();
        Environment.handleError(ex, 1, mockLoader);

        verify(specificGlobalHandler).onError(ex, true);
      }
    }


    class IllegalArgumentExceptionErrorHandler implements ErrorHandler {
      @Override
      public void onError(Throwable throwable, boolean debug) {
        // no-op
      }
    }

    class WebforjGlobalErrorHandler implements ErrorHandler {
      @Override
      public void onError(Throwable throwable, boolean debug) {
        // no-op
      }
    }
  }

  @Nested
  class Configuration {

    @AfterEach
    void teardown() {
      System.clearProperty("webforj.conf");
      Environment.cleanup();
    }

    @Test
    void shouldLoadResourceConfigWhenPropertySetToResource() throws Exception {
      System.setProperty("webforj.conf", "!!custom-config.conf");

      try (var configFactoryStatic = mockStatic(ConfigFactory.class)) {
        Config resourceConfig = mock(Config.class);
        Config defaultConfig = mock(Config.class);

        configFactoryStatic.when(() -> ConfigFactory.parseResourcesAnySyntax(any(ClassLoader.class),
            eq("custom-config.conf"))).thenReturn(resourceConfig);
        configFactoryStatic.when(() -> ConfigFactory.parseResourcesAnySyntax(any(ClassLoader.class),
            eq("webforj-default.conf"))).thenReturn(defaultConfig);

        when(resourceConfig.withFallback(defaultConfig)).thenReturn(resourceConfig);

        // Mock the required BBj objects
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));

        // Initialize environment - this will load the config
        Environment.init(mockApi, 0);

        Config config = Environment.getCurrent().getConfig();

        assertNotNull(config);
        verify(resourceConfig).withFallback(defaultConfig);
      }
    }

    @Test
    void shouldLoadFileConfigWhenPropertySetToAbsolutePath() throws Exception {
      Path tempConfigFile = Files.createTempFile("test-config", ".conf");
      System.setProperty("webforj.conf", tempConfigFile.toAbsolutePath().toString());

      try (var configFactoryStatic = mockStatic(ConfigFactory.class)) {
        Config fileConfig = mock(Config.class);
        Config defaultConfig = mock(Config.class);

        configFactoryStatic.when(() -> ConfigFactory.parseFile(tempConfigFile.toFile()))
            .thenReturn(fileConfig);
        configFactoryStatic.when(() -> ConfigFactory.parseResourcesAnySyntax(any(ClassLoader.class),
            eq("webforj-default.conf"))).thenReturn(defaultConfig);

        when(fileConfig.withFallback(defaultConfig)).thenReturn(fileConfig);

        // Mock the required BBj objects
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));

        // Initialize environment - this will load the config
        Environment.init(mockApi, 0);

        Config config = Environment.getCurrent().getConfig();

        assertNotNull(config, "Configuration should not be null.");
        verify(fileConfig).withFallback(defaultConfig);
      } finally {
        Files.deleteIfExists(tempConfigFile);
      }
    }
  }

  @Test
  void shouldCallWaitVerbWithCorrectSeconds() throws Exception {
    BBjAPI mockApi = mock(BBjAPI.class);
    BBjInterpreter mockInterpreter = mock(BBjInterpreter.class);

    when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));
    when(mockApi.getInterpreter()).thenReturn(mockInterpreter);

    Environment.init(mockApi, 0);
    Environment.getCurrent().sleep(5);

    verify(mockInterpreter).waitVerb(5);

    Environment.cleanup();
  }
}
