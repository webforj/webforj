package com.webforj.bbj.security.store.browsersecuritystore;

import static com.webforj.bbj.security.store.BrowserStoreFixture.APP;
import static com.webforj.bbj.security.store.BrowserStoreFixture.LOGGER_NAME;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createBrowserStore;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.LocalStorage;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjBrowserSecurityStoreLocationTest {
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  @TempDir
  private Path directory;
  private LocalStorage storage;
  private BBjSecurityStore store;

  @BeforeEach
  void setUp() {
    storage = mock(LocalStorage.class);
    store = createBrowserStore(() -> directory, () -> APP, () -> storage);
  }

  @Nested
  class SaveLocation {

    @Test
    void shouldSaveTheFullUriUnderTheRequestedLocationKey() {
      store.saveLocation(new Location("/protected?x=1#top"));

      verify(storage).setItem(KEY, "/protected?x=1#top");
    }
  }

  @Nested
  class ReadLocation {

    @Test
    void shouldReadTheSavedLocation() {
      when(storage.get(KEY)).thenReturn("/protected?x=1");

      Optional<Location> location = store.readLocation();

      assertEquals("/protected?x=1", location.orElseThrow().getFullURI());
      verify(storage, never()).remove(KEY);
    }

    @Test
    void shouldReadNothingWhenNoLocationIsSaved() {
      assertTrue(store.readLocation().isEmpty());

      verify(storage, never()).remove(KEY);
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldDiscardValueThatIsNotPathInsideTheApp(String value) {
      when(storage.get(KEY)).thenReturn(value);

      assertTrue(store.readLocation().isEmpty());

      verify(storage).remove(KEY);
    }

    @ParameterizedTest
    @MethodSource("controlCharacterValues")
    void shouldDiscardValueWithControlCharacter(String value) {
      when(storage.get(KEY)).thenReturn(value);

      assertTrue(store.readLocation().isEmpty());

      verify(storage).remove(KEY);
    }

    private static Stream<Arguments> invalidValues() {
      return Stream.of(Arguments.of("https://evil.example"), Arguments.of("//evil"),
          Arguments.of("/evil\\path"), Arguments.of("javascript:evil"), Arguments.of(""));
    }

    private static Stream<Arguments> controlCharacterValues() {
      return Stream.of(Arguments.of("/\t/x"), Arguments.of("/\n/x"), Arguments.of("/\r/x"),
          Arguments.of("/x\u0001"), Arguments.of("/x\u007f"));
    }
  }

  @Nested
  class ClearLocation {

    @Test
    void shouldClearTheSavedLocation() {
      store.clearLocation();

      verify(storage).remove(KEY);
    }
  }

  @Nested
  class WithoutStorage {

    @BeforeEach
    void setUp() {
      store = createBrowserStore(() -> directory, () -> APP, () -> null);
    }

    @Test
    void shouldSaveNothing() {
      Location location = new Location("/protected");

      assertDoesNotThrow(() -> store.saveLocation(location));

      verifyNoInteractions(storage);
    }

    @Test
    void shouldReadNothing() {
      assertTrue(store.readLocation().isEmpty());

      verifyNoInteractions(storage);
    }

    @Test
    void shouldClearNothing() {
      assertDoesNotThrow(store::clearLocation);

      verifyNoInteractions(storage);
    }
  }

  @Nested
  class StorageFailure {

    private final Logger logger = Logger.getLogger(LOGGER_NAME);
    private final List<LogRecord> records = new ArrayList<>();
    private final Handler handler = new Handler() {
      @Override
      public void publish(LogRecord logRecord) {
        records.add(logRecord);
      }

      @Override
      public void flush() {
        // Not implemented
      }

      @Override
      public void close() {
        // Not implemented
      }
    };
    private Level previousLevel;

    @BeforeEach
    void setUp() {
      previousLevel = logger.getLevel();
      logger.setLevel(Level.ALL);
      logger.addHandler(handler);
    }

    @AfterEach
    void tearDown() {
      logger.removeHandler(handler);
      logger.setLevel(previousLevel);
    }

    @ParameterizedTest
    @MethodSource("failingOperations")
    void shouldLogTheClassNameOnlyWhenTheStorageFails(String expected,
        Consumer<BBjSecurityStore> operation) {
      RuntimeException failure = new IllegalStateException("secret-text");
      when(storage.get(KEY)).thenThrow(failure);
      doThrow(failure).when(storage).setItem(anyString(), anyString());
      doThrow(failure).when(storage).remove(KEY);

      assertDoesNotThrow(() -> operation.accept(store));

      List<LogRecord> warnings =
          records.stream().filter(it -> it.getLevel() == Level.WARNING).toList();
      assertEquals(1, warnings.size());
      assertEquals(expected, new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    private static Stream<Arguments> failingOperations() {
      Consumer<BBjSecurityStore> save = target -> target.saveLocation(new Location("/protected"));
      Consumer<BBjSecurityStore> read = BBjSecurityStore::readLocation;
      Consumer<BBjSecurityStore> clear = BBjSecurityStore::clearLocation;

      return Stream.of(
          Arguments.of(
              "The requested location could not be saved (java.lang.IllegalStateException)", save),
          Arguments.of("The requested location could not be read (java.lang.IllegalStateException)",
              read),
          Arguments.of(
              "The requested location could not be cleared (java.lang.IllegalStateException)",
              clear));
    }
  }
}
