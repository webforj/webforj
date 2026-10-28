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
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BBjBrowserSecurityStoreClearSessionTest {
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
  void shouldRemoveTheEntryOfTheBrowserAndKeepTheOthers() throws IOException {
    Map<String, String> otherBrowser = new HashMap<>();
    createStore(createStorage(otherBrowser), NOW).saveSession(session);
    store.saveSession(session);

    store.clearSession();

    assertTrue(browser.isEmpty());
    assertEquals(List.of(otherBrowser.get(HANDLE_KEY)), readNames());
  }

  @Test
  void shouldNotCreateTheFileWhenThereIsNone() {
    browser.put(HANDLE_KEY, createName(EXPIRES_AT, 1));

    store.clearSession();

    assertTrue(browser.isEmpty());
    assertFalse(Files.exists(file));
  }

  @Test
  void shouldLeaveTheFileAloneForNameThatIsNoEntry() throws IOException {
    store.saveSession(session);
    browser.put(HANDLE_KEY, "not an entry");
    List<String> before = readLines();

    store.clearSession();

    assertTrue(browser.isEmpty());
    assertEquals(before, readLines());
  }

  @Test
  void shouldDoNothingWithoutStorage() {
    BBjSecurityStore withoutStorage = createBrowserStore(() -> directory, () -> APP, () -> null);

    assertDoesNotThrow(withoutStorage::clearSession);
  }

  @Test
  void shouldLogTheClassNameWhenTheStorageFails() throws IOException {
    store.saveSession(session);
    LocalStorage failing = createStorage(browser);
    doThrow(new IllegalStateException(TOKEN)).when(failing).remove(HANDLE_KEY, KEY_KEY);
    BBjSecurityStore failingStore = createStore(failing, NOW);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      failingStore.clearSession();
    } finally {
      logger.removeHandler(handler);
    }

    assertEquals(List.of(
        "The sign in could not be removed from the browser " + "(java.lang.IllegalStateException)"),
        messages);
    assertTrue(readLines().isEmpty());
  }

  @Test
  void shouldNotThrowWhenTheStorageCannotBeResolved() {
    BBjSecurityStore failingStore = createBrowserStore(() -> directory, () -> APP, () -> {
      throw new IllegalStateException(TOKEN);
    });

    assertDoesNotThrow(failingStore::clearSession);
  }

  @Test
  void shouldNotThrowWhenTheBrowserCannotBeRead() {
    LocalStorage failing = mock(LocalStorage.class);
    when(failing.get(List.of(HANDLE_KEY, KEY_KEY))).thenThrow(new IllegalStateException(TOKEN));
    BBjSecurityStore failingStore = createStore(failing, NOW);

    assertDoesNotThrow(failingStore::clearSession);
  }

  @Test
  void shouldLogTheClassNameWhenTheEntryCannotBeRemoved() throws IOException {
    browser.put(HANDLE_KEY, createName(EXPIRES_AT, 1));
    Files.createDirectories(file);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      store.clearSession();
    } finally {
      logger.removeHandler(handler);
    }

    assertTrue(browser.isEmpty());
    assertEquals(1, messages.size());
    assertTrue(messages.get(0).startsWith("The session entry could not be removed ("));
  }

  private BBjSecurityStore createStore(LocalStorage target, Instant now) {
    return createBrowserStore(() -> directory, () -> APP, () -> target,
        Clock.fixed(now, ZoneOffset.UTC));
  }

  private List<String> readLines() throws IOException {
    return Files.readAllLines(file);
  }

  private List<String> readNames() throws IOException {
    return readLines().stream().map(line -> line.substring(0, line.indexOf('\t'))).toList();
  }
}
