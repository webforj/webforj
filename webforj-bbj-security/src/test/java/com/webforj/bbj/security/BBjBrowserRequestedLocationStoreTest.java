package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.SessionStorage;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjBrowserRequestedLocationStoreTest {
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  @SuppressWarnings("LoggerInitializedWithForeignClass")
  private final Logger logger = Logger.getLogger(BBjBrowserRequestedLocationStore.class.getName());
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
  private final SessionStorage storage = mock(SessionStorage.class);
  private final BBjBrowserRequestedLocationStore store =
      new BBjBrowserRequestedLocationStore(() -> storage);
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

  @Test
  void shouldSaveTheFullUriUnderTheRequestedLocationKey() {
    store.saveLocation(new Location("/protected?x=1#top"));

    verify(storage).setItem(KEY, "/protected?x=1#top");
  }

  @Test
  void shouldReadTheSavedLocation() {
    when(storage.getItem(KEY)).thenReturn("/protected?x=1");

    Optional<Location> location = store.readLocation();

    assertEquals("/protected?x=1", location.orElseThrow().getFullURI());
    verify(storage, never()).removeItem(KEY);
    assertTrue(records.isEmpty());
  }

  @Test
  void shouldReadNothingWhenNoLocationIsSaved() {
    assertTrue(store.readLocation().isEmpty());

    verify(storage, never()).removeItem(KEY);
    assertTrue(records.isEmpty());
  }

  @ParameterizedTest
  @MethodSource("invalidValues")
  void shouldDiscardAnInvalidValueAndWarnOnceWithoutTheValue(String value) {
    when(storage.getItem(KEY)).thenReturn(value);

    assertTrue(store.readLocation().isEmpty());

    verify(storage).removeItem(KEY);
    List<LogRecord> warnings =
        records.stream().filter(record -> record.getLevel() == Level.WARNING).toList();
    assertEquals(1, warnings.size());
    assertFalse(warnings.get(0).getMessage().contains("evil"));
    assertNull(warnings.get(0).getParameters());
  }

  @Test
  void shouldClearTheSavedLocation() {
    store.clearLocation();

    verify(storage).removeItem(KEY);
  }

  @Test
  void shouldReadNothingAndSaveNothingWithoutStorage() {
    Supplier<SessionStorage> none = () -> null;
    BBjBrowserRequestedLocationStore unavailable = new BBjBrowserRequestedLocationStore(none);

    assertDoesNotThrow(() -> unavailable.saveLocation(new Location("/protected")));
    assertTrue(unavailable.readLocation().isEmpty());
    assertDoesNotThrow(unavailable::clearLocation);
  }

  private static Stream<Arguments> invalidValues() {
    return Stream.of(Arguments.of("https://evil.example"), Arguments.of("//evil"),
        Arguments.of("/evil\\path"), Arguments.of("javascript:evil"), Arguments.of(""));
  }
}
