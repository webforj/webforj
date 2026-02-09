package com.webforj.i18n;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Request;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class LocaleAutoDetectListenerTest {

  private LocaleAutoDetectListener listener;
  private App mockApp;
  private Environment mockEnv;
  private Config mockConfig;
  private Request mockRequest;

  @BeforeEach
  void setUp() {
    listener = new LocaleAutoDetectListener();
    mockApp = mock(App.class);
    mockEnv = mock(Environment.class);
    mockConfig = mock(Config.class);
    mockRequest = mock(Request.class);

    when(mockEnv.getConfig()).thenReturn(mockConfig);
  }

  void configureAutoDetect(boolean enabled) {
    when(mockConfig.hasPath("webforj.i18n.auto-detect")).thenReturn(true);
    when(mockConfig.getIsNull("webforj.i18n.auto-detect")).thenReturn(false);
    when(mockConfig.getBoolean("webforj.i18n.auto-detect")).thenReturn(enabled);
  }

  void configureSupportedLocales(List<String> locales) {
    when(mockConfig.hasPath("webforj.i18n.supported-locales")).thenReturn(true);
    when(mockConfig.getIsNull("webforj.i18n.supported-locales")).thenReturn(false);
    when(mockConfig.getStringList("webforj.i18n.supported-locales")).thenReturn(locales);
  }

  @Nested
  class WhenAutoDetectDisabled {

    @Test
    void shouldNotSetLocaleWhenAutoDetectIsFalse() {
      configureAutoDetect(false);

      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(mockEnv);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(any(Locale.class)), never());
      }
    }

    @Test
    void shouldNotSetLocaleWhenAutoDetectConfigMissing() {
      when(mockConfig.hasPath("webforj.i18n.auto-detect")).thenReturn(false);

      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(mockEnv);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(any(Locale.class)), never());
      }
    }

    @Test
    void shouldNotSetLocaleWhenEnvironmentIsNull() {
      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(null);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(any(Locale.class)), never());
      }
    }
  }

  @Nested
  class WhenSupportedLocalesEmpty {

    @Test
    void shouldNotSetLocaleWhenSupportedLocalesEmpty() {
      configureAutoDetect(true);
      configureSupportedLocales(Collections.emptyList());

      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(mockEnv);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(any(Locale.class)), never());
      }
    }

    @Test
    void shouldNotSetLocaleWhenSupportedLocalesConfigMissing() {
      configureAutoDetect(true);
      when(mockConfig.hasPath("webforj.i18n.supported-locales")).thenReturn(false);

      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(mockEnv);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(any(Locale.class)), never());
      }
    }
  }

  @Nested
  class LocaleResolution {

    @BeforeEach
    void setUpAutoDetectEnabled() {
      configureAutoDetect(true);
    }

    static Stream<Arguments> localeResolutionCases() {
      // @formatter:off
      return Stream.of(
          // Exact match
          Arguments.of(
              Arrays.asList("en", "fr", "de"),
              Arrays.asList(Locale.FRENCH, Locale.ENGLISH),
              Locale.FRENCH),
          // Language match (fr-CA -> fr)
          Arguments.of(
              Arrays.asList("en", "fr", "de"),
              Arrays.asList(Locale.CANADA_FRENCH),
              Locale.FRENCH),
          // No match -> first supported
          Arguments.of(
              Arrays.asList("en", "fr", "de"),
              Arrays.asList(Locale.JAPANESE),
              Locale.ENGLISH),
          // Empty browser preferences -> first supported
          Arguments.of(
              Arrays.asList("de", "fr", "en"),
              Collections.emptyList(),
              Locale.GERMAN),
          // Null browser preferences -> first supported
          Arguments.of(
              Arrays.asList("fr", "de", "en"),
              null,
              Locale.FRENCH),
          // Exact match preferred over language match
          Arguments.of(
              Arrays.asList("en-US", "en-GB", "fr"),
              Arrays.asList(Locale.UK, Locale.US),
              Locale.UK),
          // Browser preference order respected
          Arguments.of(
              Arrays.asList("en", "fr", "de"),
              Arrays.asList(new Locale("es"), Locale.GERMAN, Locale.FRENCH),
              Locale.GERMAN)
      );
      // @formatter:on
    }

    @ParameterizedTest
    @MethodSource("localeResolutionCases")
    void shouldResolveLocale(List<String> supported, List<Locale> preferred, Locale expected) {
      configureSupportedLocales(supported);
      when(mockRequest.getPreferredLocales()).thenReturn(preferred);

      try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
          MockedStatic<Request> requestMock = mockStatic(Request.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        envMock.when(Environment::getCurrent).thenReturn(mockEnv);
        requestMock.when(Request::getCurrent).thenReturn(mockRequest);

        listener.onWillRun(mockApp);

        appMock.verify(() -> App.setLocale(expected));
      }
    }
  }
}
