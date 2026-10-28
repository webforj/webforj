package com.webforj.bbj.security.store.browsersecuritystore;

import static com.webforj.bbj.security.store.BrowserStoreFixture.APP;
import static com.webforj.bbj.security.store.BrowserStoreFixture.EXPIRES_AT;
import static com.webforj.bbj.security.store.BrowserStoreFixture.HANDLE_KEY;
import static com.webforj.bbj.security.store.BrowserStoreFixture.KEY_KEY;
import static com.webforj.bbj.security.store.BrowserStoreFixture.LOGGER_NAME;
import static com.webforj.bbj.security.store.BrowserStoreFixture.NOW;
import static com.webforj.bbj.security.store.BrowserStoreFixture.TOKEN;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createApplicationKey;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createBrowserStore;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createHandler;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createName;
import static com.webforj.bbj.security.store.BrowserStoreFixture.createStorage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.webstorage.LocalStorage;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
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
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjBrowserSecurityStoreSaveSessionTest {
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
  void shouldKeepTheSessionInTheFileAndThePairInTheBrowser() throws IOException {
    store.saveSession(session);

    List<String> lines = readLines();
    assertEquals(1, lines.size());
    String name = browser.get(HANDLE_KEY);
    assertTrue(lines.get(0).startsWith(name + "\t"));
    assertEquals("v1." + EXPIRES_AT.toEpochMilli() + ".", name.substring(0, name.length() - 43));
    assertFalse(lines.get(0).contains(TOKEN));
    assertFalse(browser.get(KEY_KEY).isBlank());
  }

  @Test
  void shouldCreateFolderAndFileForTheOwnerOnly() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));

    store.saveSession(session);

    assertEquals("rw-------", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
    assertEquals("rwx------",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getParent())));
  }

  @Test
  void shouldReplaceThePreviousEntryOfTheBrowser() throws IOException {
    store.saveSession(session);
    String first = browser.get(HANDLE_KEY);

    store.saveSession(session);

    assertEquals(1, readLines().size());
    assertNotEquals(first, browser.get(HANDLE_KEY));
    assertEquals(List.of(browser.get(HANDLE_KEY)), readNames());
  }

  @Test
  void shouldKeepTheEntryOfAnotherBrowser() throws IOException {
    Map<String, String> otherBrowser = new HashMap<>();
    createStore(createStorage(otherBrowser), NOW).saveSession(session);

    store.saveSession(session);

    assertEquals(Set.of(browser.get(HANDLE_KEY), otherBrowser.get(HANDLE_KEY)),
        Set.copyOf(readNames()));
  }

  @Test
  void shouldDropExpiredEntriesAndLinesThatAreNoEntries() throws IOException {
    String live = createName(EXPIRES_AT, 1);
    Files.createDirectories(file.getParent());
    Files.write(file,
        List.of(createName(NOW, 2) + "\tAAAA", createName(NOW.minusSeconds(1), 3) + "\tAAAA",
            live + "\tAAAA", "no entry", "v1.1.short\tAAAA", createName(EXPIRES_AT, 4)));

    store.saveSession(session);

    assertEquals(Set.of(live, browser.get(HANDLE_KEY)), Set.copyOf(readNames()));
  }

  @Test
  void shouldEvictTheEarliestExpiryAtTheLimit() throws IOException {
    List<String> lines = new ArrayList<>();
    for (int index = 0; index < 10000; index++) {
      lines.add(createName(EXPIRES_AT.plusSeconds(index + 1), index) + "\tAAAA");
    }
    Files.createDirectories(file.getParent());
    Files.write(file, lines);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      store.saveSession(session);
    } finally {
      logger.removeHandler(handler);
    }

    List<String> names = readNames();
    assertEquals(10000, names.size());
    assertFalse(names.contains(createName(EXPIRES_AT.plusSeconds(1), 0)));
    assertTrue(names.contains(createName(EXPIRES_AT.plusSeconds(2), 1)));
    assertTrue(names.contains(browser.get(HANDLE_KEY)));
    assertEquals(
        List.of("The session store removed 1 sessions because its limit of 10000 was reached"),
        messages);
  }

  @Test
  void shouldFailWithoutStorage() {
    BBjSecurityStore withoutStorage = createBrowserStore(() -> directory, () -> APP, () -> null);

    IllegalStateException thrown =
        assertThrows(IllegalStateException.class, () -> withoutStorage.saveSession(session));

    assertEquals("The sign in could not be kept", thrown.getMessage());
  }

  @Test
  void shouldFailWhenTheFolderCannotBeCreated() throws IOException {
    Path blocker = Files.createFile(directory.resolve("blocker"));
    BBjSecurityStore blocked = createBrowserStore(() -> blocker, () -> APP, () -> storage);

    IllegalStateException thrown =
        assertThrows(IllegalStateException.class, () -> blocked.saveSession(session));

    assertEquals("The sign in could not be kept", thrown.getMessage());
    assertTrue(browser.isEmpty());
  }

  @Test
  void shouldFailWhenTheDirectoryCannotBeResolved() {
    BBjSecurityStore broken = createBrowserStore(() -> {
      throw new IllegalArgumentException("secret-path");
    }, () -> APP, () -> storage);

    IllegalStateException thrown =
        assertThrows(IllegalStateException.class, () -> broken.saveSession(session));

    assertEquals("The sign in could not be kept", thrown.getMessage());
  }

  @Test
  void shouldFailWhenTheSessionCannotBeEncrypted() {
    BBjSession large = new BBjSession(session.getPrincipal(), "t".repeat(70000), EXPIRES_AT);

    IllegalStateException thrown =
        assertThrows(IllegalStateException.class, () -> store.saveSession(large));

    assertEquals("The sign in could not be encrypted", thrown.getMessage());
    assertTrue(browser.isEmpty());
  }

  @Test
  void shouldRestoreThePreviousPairAndEntryWhenTheBrowserCannotBeWritten() throws IOException {
    store.saveSession(session);
    Map<String, String> previous = Map.copyOf(browser);
    LocalStorage failing = createStorage(browser);
    doThrow(new IllegalStateException("denied")).doAnswer(call -> {
      browser.putAll(call.<Map<String, String>>getArgument(0));
      return failing;
    }).when(failing).add(anyMap());
    BBjSecurityStore failingStore = createStore(failing, NOW);

    assertThrows(IllegalStateException.class, () -> failingStore.saveSession(session));

    assertEquals(previous, browser);
    assertEquals(List.of(previous.get(HANDLE_KEY)), readNames());
  }

  @Test
  void shouldLeaveTheBrowserEmptyWhenItHoldsNoPreviousPairAndCannotBeWritten() throws IOException {
    LocalStorage failing = createStorage(browser);
    doThrow(new IllegalStateException("denied")).when(failing).add(anyMap());
    BBjSecurityStore failingStore = createStore(failing, NOW);

    assertThrows(IllegalStateException.class, () -> failingStore.saveSession(session));

    assertTrue(browser.isEmpty());
    assertTrue(readLines().isEmpty());
  }

  @Test
  void shouldStillSaveWhenTheSweepFails() throws IOException {
    Clock failing = mock(Clock.class);
    when(failing.instant()).thenThrow(new IllegalStateException(TOKEN));
    BBjSecurityStore failingStore =
        createBrowserStore(() -> directory, () -> APP, () -> storage, failing);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      failingStore.saveSession(session);
    } finally {
      logger.removeHandler(handler);
    }

    assertEquals(List.of(browser.get(HANDLE_KEY)), readNames());
    assertEquals(List.of("Expired sign ins could not be removed (java.lang.IllegalStateException)"),
        messages);
  }

  @Test
  void shouldAcceptTheConstructorWithTheSystemClock() {
    BBjSession later =
        new BBjSession(session.getPrincipal(), TOKEN, Instant.now().plusSeconds(3600));
    BBjSecurityStore system = createBrowserStore(() -> directory, () -> APP, () -> storage);

    system.saveSession(later);

    assertEquals(TOKEN, system.readSession().orElseThrow().getToken());
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

  @Nested
  class CreateApplicationKey {

    @ParameterizedTest
    @MethodSource("applicationNames")
    void shouldCreateFileNameSafeKey(String applicationName, String expected) {
      assertEquals(expected, createApplicationKey(applicationName));
    }

    private static Stream<Arguments> applicationNames() {
      return Stream.of(Arguments.of("shop", "shop"), Arguments.of("My_app-2", "My_app-2"),
          Arguments.of("my app", "my_app-cccdfa68"), Arguments.of("", "app-e3b0c442"),
          Arguments.of("x".repeat(70), "x".repeat(64) + "-c71bd109"));
    }
  }
}
