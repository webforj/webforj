package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.fixture.FirstOwnerEvaluator;
import com.webforj.bbj.security.fixture.LateOwnerEvaluator;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import org.junit.jupiter.api.Test;

class BBjRouteSecurityEvaluatorLoaderTest {

  @Test
  void shouldReturnNothingWhenNoServiceFileIsFound() throws IOException {
    try (URLClassLoader loader = new URLClassLoader(new URL[0], getClass().getClassLoader())) {
      assertTrue(BBjRouteSecurityEvaluatorLoader.loadEvaluators(loader).isEmpty());
    }
  }

  @Test
  void shouldKeepTheServiceFileOrderForEqualPriorities() throws IOException {
    try (URLClassLoader loader = createFixtureLoader("several")) {
      List<BBjRouteSecurityEvaluator> evaluators =
          BBjRouteSecurityEvaluatorLoader.loadEvaluators(loader);

      assertEquals(List.of("zulu", "alpha", "mike"),
          evaluators.stream()
              .map(evaluator -> evaluator.evaluate(String.class, null, null, null).getReason())
              .toList());
    }
  }

  @Test
  void shouldNotLoadAPlainEvaluatorListedUnderTheRouteSecurityEvaluatorName() throws IOException {
    try (URLClassLoader loader = createFixtureLoader("plain")) {
      assertTrue(BBjRouteSecurityEvaluatorLoader.loadEvaluators(loader).isEmpty());
    }
  }

  @Test
  void shouldFailWhenProviderCannotBeCreated() throws IOException {
    try (URLClassLoader loader = createFixtureLoader("broken")) {
      ServiceConfigurationError error = assertThrows(ServiceConfigurationError.class,
          () -> BBjRouteSecurityEvaluatorLoader.loadEvaluators(loader));

      assertTrue(error.getMessage().contains("BrokenOwnerEvaluator"));
    }
  }

  @Test
  void shouldLogAnErrorWhenProviderCannotBeCreated() throws IOException {
    Logger julLogger = Logger.getLogger(BBjRouteSecurityEvaluatorLoader.class.getName());
    List<LogRecord> records = new ArrayList<>();
    Handler handler = new Handler() {
      @Override
      public void publish(LogRecord record) {
        records.add(record);
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    };
    julLogger.addHandler(handler);

    try (URLClassLoader loader = createFixtureLoader("broken")) {
      assertThrows(ServiceConfigurationError.class,
          () -> BBjRouteSecurityEvaluatorLoader.loadEvaluators(loader));
    } finally {
      julLogger.removeHandler(handler);
    }

    assertEquals(1, records.size());
    assertEquals(Level.SEVERE, records.get(0).getLevel());
  }

  @Test
  void shouldLoadFromTheClassLoaderOfTheLibraryWhenTheClassLoaderIsNull() throws Exception {
    try (URLClassLoader libraryLoader = createLibraryLoader("several")) {
      assertEquals(
          List.of("com.webforj.bbj.security.fixture.ZuluOwnerEvaluator",
              "com.webforj.bbj.security.fixture.AlphaOwnerEvaluator",
              "com.webforj.bbj.security.fixture.MikeOwnerEvaluator"),
          loadEvaluatorNames(libraryLoader, null));
    }
  }

  @Test
  void shouldNotUseTheClassLoaderOfTheLibraryWhenTheClassLoaderIsGiven() throws Exception {
    try (URLClassLoader libraryLoader = createLibraryLoader("several");
        URLClassLoader loader = createFixtureLoader("plain")) {
      assertTrue(loadEvaluatorNames(libraryLoader, loader).isEmpty());
    }
  }

  private List<String> loadEvaluatorNames(ClassLoader libraryLoader, ClassLoader classLoader)
      throws ReflectiveOperationException {
    Class<?> loaderClass =
        Class.forName(BBjRouteSecurityEvaluatorLoader.class.getName(), true, libraryLoader);
    Method method = loaderClass.getDeclaredMethod("loadEvaluators", ClassLoader.class);
    method.setAccessible(true);
    List<?> evaluators = (List<?>) method.invoke(null, classLoader);

    return evaluators.stream().map(evaluator -> evaluator.getClass().getName()).toList();
  }

  private URLClassLoader createLibraryLoader(String fixture) {
    URL folder = getClass().getResource("/fixtures/custom-evaluators/" + fixture + "/");
    URL[] classPath = Arrays.stream(System.getProperty("java.class.path").split(File.pathSeparator))
        .map(entry -> {
          try {
            return new File(entry).toURI().toURL();
          } catch (MalformedURLException e) {
            throw new IllegalStateException(e);
          }
        }).toArray(URL[]::new);
    URL[] urls = Arrays.copyOf(classPath, classPath.length + 1);
    urls[classPath.length] = folder;

    return new URLClassLoader(urls, ClassLoader.getPlatformClassLoader());
  }

  private URLClassLoader createFixtureLoader(String fixture) {
    URL folder = getClass().getResource("/fixtures/custom-evaluators/" + fixture + "/");

    return new URLClassLoader(new URL[] {folder}, getClass().getClassLoader());
  }
}
