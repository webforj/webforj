package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.typesafe.config.Config;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjException;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BootstrapTest {

  Environment environment;
  Conceiver conceiver;
  MockedStatic<Environment> mockedEnvironment;
  MockedStatic<ConceiverProvider> mockedConceiverProvider;

  @BeforeEach
  void createMocks() {
    environment = mock(Environment.class);
    conceiver = mock(Conceiver.class);
    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    mockedConceiverProvider.when(ConceiverProvider::getCurrent).thenReturn(conceiver);
  }

  @AfterEach
  void closeMocks() {
    mockedConceiverProvider.close();
    mockedEnvironment.close();
  }

  @Test
  void shouldInitializeApplicationClass() throws Exception {
    App app = mock(App.class);
    when(conceiver.getApplication(EntryApp.class)).thenReturn(app);
    BBjAPI api = mock(BBjAPI.class);
    String className = EntryApp.class.getName();

    App result = Bootstrap.init(api, 0, className);

    assertSame(app, result);
    verify(app).initialize();
  }

  @ParameterizedTest
  @MethodSource("createRejectedClassArguments")
  void shouldRejectEntryClassBeforeCreatingIt(String className, String expectedMessage)
      throws Exception {
    BBjAPI api = mock(BBjAPI.class);

    WebforjAppInitializeException exception =
        assertThrows(WebforjAppInitializeException.class, () -> Bootstrap.init(api, 0, className));

    assertEquals(expectedMessage, exception.getMessage());
    verify(conceiver, never()).getApplication(any());
  }

  @Test
  void shouldRejectEntryClassFromConfiguration() throws Exception {
    String className = String.class.getName();
    Config config = mock(Config.class);
    when(environment.getConfig()).thenReturn(config);
    when(config.hasPath("webforj.entry")).thenReturn(true);
    when(config.getIsNull("webforj.entry")).thenReturn(false);
    when(config.getString("webforj.entry")).thenReturn(className);
    BBjAPI api = mock(BBjAPI.class);

    WebforjAppInitializeException exception =
        assertThrows(WebforjAppInitializeException.class, () -> Bootstrap.init(api, 0, null));

    assertEquals("Application class '%s' must extend %s.".formatted(className, App.class.getName()),
        exception.getMessage());
    verify(conceiver, never()).getApplication(any());
  }

  @Test
  void shouldRejectClassThatCannotBeFound() {
    BBjAPI api = mock(BBjAPI.class);
    String className = "MissingEntryApp";

    WebforjAppInitializeException exception =
        assertThrows(WebforjAppInitializeException.class, () -> Bootstrap.init(api, 0, className));

    assertEquals("Failed to find application class 'MissingEntryApp'."
        + " Ensure the class is in the classpath.", exception.getMessage());
  }

  private static Stream<Arguments> createRejectedClassArguments() {
    return Stream.of(
        Arguments.of(String.class.getName(),
            "Application class '%s' must extend %s.".formatted(String.class.getName(),
                App.class.getName())),
        Arguments.of(InitializerFailingClass.class.getName(),
            "Application class '%s' must extend %s."
                .formatted(InitializerFailingClass.class.getName(), App.class.getName())),
        Arguments.of(App.class.getName(),
            "Application class '%s' must not be abstract.".formatted(App.class.getName())));
  }

  static class EntryApp extends App {
    @Override
    public void run() throws WebforjException {}
  }

  static final class InitializerFailingClass {
    static final Object VALUE = createValue();

    private static Object createValue() {
      throw new IllegalStateException();
    }
  }
}
