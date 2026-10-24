package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.SessionStorage;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BBjBrowserRequestedLocationStoreTest {
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  private MockedStatic<Environment> environmentMock;
  private MockedStatic<SessionStorage> storageMock;
  private SessionStorage storage;
  private BBjBrowserRequestedLocationStore store;

  @BeforeEach
  void setUp() {
    storage = mock(SessionStorage.class);
    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    storageMock = mockStatic(SessionStorage.class);
    storageMock.when(SessionStorage::getCurrent).thenReturn(storage);
    store = new BBjBrowserRequestedLocationStore();
  }

  @AfterEach
  void tearDown() {
    storageMock.close();
    environmentMock.close();
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
    void shouldDiscardAValueThatIsNotAPathInsideTheApp(String value) {
      when(storage.get(KEY)).thenReturn(value);

      assertTrue(store.readLocation().isEmpty());

      verify(storage).remove(KEY);
    }

    @ParameterizedTest
    @MethodSource("controlCharacterValues")
    void shouldDiscardAValueWithAControlCharacter(String value) {
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
  class WithoutEnvironment {

    @BeforeEach
    void setUp() {
      environmentMock.when(Environment::getCurrent).thenReturn(null);
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

    private final Logger logger =
        Logger.getLogger(BBjBrowserRequestedLocationStore.class.getName());
    private final List<LogRecord> records = new ArrayList<>();
    private final Handler handler = new Handler() {
      @Override
      public void publish(LogRecord record) {
        records.add(record);
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
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
        Consumer<BBjBrowserRequestedLocationStore> operation) {
      RuntimeException failure = new IllegalStateException("secret-text");
      when(storage.get(KEY)).thenThrow(failure);
      doThrow(failure).when(storage).setItem(anyString(), anyString());
      doThrow(failure).when(storage).remove(KEY);

      assertDoesNotThrow(() -> operation.accept(store));

      List<LogRecord> warnings =
          records.stream().filter(record -> record.getLevel() == Level.WARNING).toList();
      assertEquals(1, warnings.size());
      assertEquals(expected, new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    private static Stream<Arguments> failingOperations() {
      Consumer<BBjBrowserRequestedLocationStore> save =
          target -> target.saveLocation(new Location("/protected"));
      Consumer<BBjBrowserRequestedLocationStore> read =
          BBjBrowserRequestedLocationStore::readLocation;
      Consumer<BBjBrowserRequestedLocationStore> clear =
          BBjBrowserRequestedLocationStore::clearLocation;

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
