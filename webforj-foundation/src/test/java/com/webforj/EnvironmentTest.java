package com.webforj;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import com.webforj.error.ErrorHandler;
import com.webforj.error.GlobalErrorHandler;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
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
        when(resourceConfig.resolve()).thenReturn(resourceConfig);

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
        when(fileConfig.resolve()).thenReturn(fileConfig);

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

    @ParameterizedTest
    @MethodSource("getOptionalSubstitutionCases")
    void shouldResolveOptionalSubstitution(String content, String expected) throws Exception {
      Path tempConfigFile = Files.createTempFile("test-config", ".conf");
      Files.writeString(tempConfigFile, content);
      System.setProperty("webforj.conf", tempConfigFile.toAbsolutePath().toString());

      try {
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));

        Environment.init(mockApi, 0);

        assertEquals(expected, Environment.getCurrent().getConfig().getString("test.value"));
      } finally {
        Files.deleteIfExists(tempConfigFile);
      }
    }

    @Test
    void shouldResolveSubstitutionAgainstLaterLayer() throws Exception {
      Path tempConfigFile = Files.createTempFile("test-config", ".conf");
      Files.writeString(tempConfigFile, """
          test.name = "file"
          test.greeting = ${test.name}
          """);
      System.setProperty("webforj.conf", tempConfigFile.toAbsolutePath().toString());

      try {
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));

        Environment.init(mockApi, 0);
        Environment.getCurrent().setConfig(ConfigFactory.parseString("test.name = \"later\""));

        assertEquals("later", Environment.getCurrent().getConfig().getString("test.greeting"));
      } finally {
        Files.deleteIfExists(tempConfigFile);
      }
    }

    @Test
    void shouldFailOnMissingRequiredSubstitution() throws Exception {
      Path tempConfigFile = Files.createTempFile("test-config", ".conf");
      Files.writeString(tempConfigFile, """
          test.value = ${WEBFORJ_TEST_UNSET_VARIABLE_4711}
          """);
      System.setProperty("webforj.conf", tempConfigFile.toAbsolutePath().toString());

      try {
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));
        int debug = 0;

        assertThrows(ConfigException.UnresolvedSubstitution.class,
            () -> Environment.init(mockApi, debug));
      } finally {
        Files.deleteIfExists(tempConfigFile);
      }
    }

    @Test
    void shouldKeepPreviousConfigWhenLayerFailsToResolve() throws Exception {
      Path tempConfigFile = Files.createTempFile("test-config", ".conf");
      Files.writeString(tempConfigFile, """
          test.value = "file"
          """);
      System.setProperty("webforj.conf", tempConfigFile.toAbsolutePath().toString());

      try {
        BBjAPI mockApi = mock(BBjAPI.class);
        when(mockApi.openSysGui(anyString())).thenReturn(mock(BBjSysGui.class));

        Environment.init(mockApi, 0);
        Config failingLayer =
            ConfigFactory.parseString("test.broken = ${WEBFORJ_TEST_UNSET_VARIABLE_4711}");
        Config validLayer = ConfigFactory.parseString("test.other = \"later\"");

        assertThrows(ConfigException.UnresolvedSubstitution.class,
            () -> Environment.getCurrent().setConfig(failingLayer));
        assertEquals("file", Environment.getCurrent().getConfig().getString("test.value"));

        assertDoesNotThrow(() -> Environment.getCurrent().setConfig(validLayer));
        assertEquals("file", Environment.getCurrent().getConfig().getString("test.value"));
        assertEquals("later", Environment.getCurrent().getConfig().getString("test.other"));
      } finally {
        Files.deleteIfExists(tempConfigFile);
      }
    }

    private static Stream<Arguments> getOptionalSubstitutionCases() {
      return Stream.of(Arguments.of("""
          test.value = ${?PATH}
          """, System.getenv("PATH")), Arguments.of("""
          test.value = "fromFile"
          test.value = ${?WEBFORJ_TEST_UNSET_VARIABLE_4711}
          """, "fromFile"));
    }
  }

  @Nested
  class Origin {

    @AfterEach
    void teardown() {
      System.clearProperty("webforj.origin");
    }

    @Test
    void shouldReturnNullWhenOriginIsUnset() {
      assertNull(Environment.getOrigin());
    }

    @Test
    void shouldReadOriginFromSystemProperty() {
      System.setProperty("webforj.origin", "https://demo.example");

      assertEquals("https://demo.example", Environment.getOrigin());
    }

    @Test
    void shouldTrimTrailingSlashesFromOrigin() {
      System.setProperty("webforj.origin", "https://demo.example//");

      assertEquals("https://demo.example", Environment.getOrigin());
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
