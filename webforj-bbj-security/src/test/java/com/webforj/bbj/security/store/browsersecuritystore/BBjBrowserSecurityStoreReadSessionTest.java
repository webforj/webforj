package com.webforj.bbj.security.store.browsersecuritystore;

import static com.webforj.bbj.security.store.BrowserStoreFixture.APP;
import static com.webforj.bbj.security.store.BrowserStoreFixture.EXPIRES_AT;
import static com.webforj.bbj.security.store.BrowserStoreFixture.HANDLE_KEY;
import static com.webforj.bbj.security.store.BrowserStoreFixture.KEY_KEY;
import static com.webforj.bbj.security.store.BrowserStoreFixture.LOGGER_NAME;
import static com.webforj.bbj.security.store.BrowserStoreFixture.NOW;
import static com.webforj.bbj.security.store.BrowserStoreFixture.TOKEN;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createBrowserStore;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createHandler;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createName;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createStorage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.webstorage.LocalStorage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Handler;
import java.util.logging.Logger;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjBrowserSecurityStoreReadSessionTest {
  @TempDir
  private Path directory;

  private Map<String, String> browser;
  private LocalStorage storage;
  private BBjSecurityStore store;
  private BBjSession session;
  private Path file;

  @BeforeEach
  void setUp() {
    browser = new HashMap<>();
    storage = createStorage(browser);
    store = createStore(storage, NOW);
    session = new BBjSession(new BBjPrincipal("alice", Set.of("ADMIN"), Set.of("read")), TOKEN,
        EXPIRES_AT);
    file = directory.resolve("webforj-bbj-security").resolve("shop.sessions");
  }

  @Test
  void shouldReadTheSavedSession() {
    store.saveSession(session);

    BBjSession read = store.readSession().orElseThrow();

    assertEquals("alice", read.getPrincipal().getUserName());
    assertEquals(Set.of("ADMIN"), read.getPrincipal().getRoles());
    assertEquals(Set.of("read"), read.getPrincipal().getAuthorities());
    assertEquals(TOKEN, read.getToken());
    assertEquals(EXPIRES_AT, read.getExpiresAt());
  }

  @Test
  void shouldReadNothingWithoutStorage() {
    BBjSecurityStore withoutStorage = createBrowserStore(() -> directory, () -> APP, () -> null);

    assertTrue(withoutStorage.readSession().isEmpty());
  }

  @Test
  void shouldReadNothingAndTouchNothingWhenTheBrowserHoldsNoPair() {
    assertTrue(store.readSession().isEmpty());
    assertFalse(Files.exists(file));
  }

  @Test
  void shouldNeitherReadNorRemoveThePairOfAnotherApplication() {
    BBjSecurityStore other = createBrowserStore(() -> directory, () -> "admin", () -> storage,
        Clock.fixed(NOW, ZoneOffset.UTC));
    other.saveSession(session);
    Map<String, String> saved = Map.copyOf(browser);

    assertTrue(store.readSession().isEmpty());

    assertEquals(saved, browser);
    assertEquals(TOKEN, other.readSession().orElseThrow().getToken());
  }

  @Test
  void shouldRemoveAnExpiredEntryWithoutReadingIt() throws IOException {
    store.saveSession(session);
    BBjSecurityStore later = createStore(storage, EXPIRES_AT);

    assertTrue(later.readSession().isEmpty());

    assertTrue(browser.isEmpty());
    assertTrue(readLines().isEmpty());
  }

  @Test
  void shouldRemoveThePairWhenTheEntryIsGone() throws IOException {
    store.saveSession(session);
    Files.write(file, List.of());

    assertTrue(store.readSession().isEmpty());

    assertTrue(browser.isEmpty());
  }

  @Test
  void shouldRemoveThePairWhenTheFileIsMissing() throws IOException {
    store.saveSession(session);
    Files.delete(file);

    assertTrue(store.readSession().isEmpty());

    assertTrue(browser.isEmpty());
    assertFalse(Files.exists(file));
  }

  @ParameterizedTest
  @MethodSource("unusablePairs")
  void shouldRemovePairThatIsNotUsable(Map<String, String> pair) {
    browser.putAll(pair);

    assertTrue(store.readSession().isEmpty());

    assertTrue(browser.isEmpty());
  }

  @Test
  void shouldRemoveTheEntryAndWarnWhenTheKeyOfTheBrowserWasChanged() throws IOException {
    store.saveSession(session);
    browser.put(KEY_KEY, "A".repeat(43));
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      assertTrue(store.readSession().isEmpty());
    } finally {
      logger.removeHandler(handler);
    }

    assertTrue(browser.isEmpty());
    assertTrue(readLines().isEmpty());
    assertEquals(List.of("A stored sign in could not be read and was removed"), messages);
  }

  @Test
  void shouldRemoveTheEntryWhenItsValueWasChanged() throws IOException {
    store.saveSession(session);
    String line = readLines().get(0);
    int split = line.indexOf('\t') + 1;
    char first = line.charAt(split);
    Files.write(file,
        List.of(line.substring(0, split) + (first == 'A' ? 'B' : 'A') + line.substring(split + 1)));

    assertTrue(store.readSession().isEmpty());

    assertTrue(browser.isEmpty());
    assertTrue(readLines().isEmpty());
  }

  @Test
  void shouldLogTheClassNameAndKeepTheBrowserWhenTheBrowserCannotBeRead() {
    LocalStorage failing = mock(LocalStorage.class);
    when(failing.get(List.of(HANDLE_KEY, KEY_KEY))).thenThrow(new IllegalStateException(TOKEN));
    BBjSecurityStore failingStore = createStore(failing, NOW);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      assertTrue(failingStore.readSession().isEmpty());
    } finally {
      logger.removeHandler(handler);
    }

    assertEquals(List.of("The stored sign in could not be read (java.lang.IllegalStateException)"),
        messages);
  }

  private static Stream<Arguments> unusablePairs() {
    return Stream.of(Arguments.of(Map.of(HANDLE_KEY, createName(EXPIRES_AT, 1))),
        Arguments.of(Map.of(HANDLE_KEY, "not an entry", KEY_KEY, "A".repeat(43))),
        Arguments.of(Map.of(HANDLE_KEY, createName(NOW, 1), KEY_KEY, "A".repeat(43))));
  }

  private BBjSecurityStore createStore(LocalStorage target, Instant now) {
    return createBrowserStore(() -> directory, () -> APP, () -> target,
        Clock.fixed(now, ZoneOffset.UTC));
  }

  private List<String> readLines() throws IOException {
    return Files.readAllLines(file);
  }

}
