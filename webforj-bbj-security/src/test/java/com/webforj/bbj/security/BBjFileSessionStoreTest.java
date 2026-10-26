package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
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
import com.webforj.webstorage.SessionStorage;
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
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjFileSessionStoreTest {
  private static final String HANDLE_KEY = "webforj-bbj-handle";
  private static final String KEY_KEY = "webforj-bbj-key";
  private static final String APP = "shop";
  private static final String TOKEN = "token-secret-123";
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  private static final Instant EXPIRES_AT = NOW.plusSeconds(3600);

  @TempDir
  private Path directory;

  private Map<String, String> tab;
  private SessionStorage storage;
  private BBjFileSessionStore store;
  private BBjSession session;
  private Path file;

  @BeforeEach
  void setUp() {
    tab = new HashMap<>();
    storage = createStorage(tab);
    store = createStore(storage, NOW);
    session = new BBjSession(new BBjPrincipal("alice", Set.of("ADMIN"), Set.of("read")), TOKEN,
        EXPIRES_AT);
    file = directory.resolve("webforj-bbj-security").resolve("shop.sessions");
  }

  private BBjFileSessionStore createStore(SessionStorage target, Instant now) {
    return new BBjFileSessionStore(() -> directory, () -> APP, () -> target,
        Clock.fixed(now, ZoneOffset.UTC));
  }

  private static SessionStorage createStorage(Map<String, String> values) {
    SessionStorage target = mock(SessionStorage.class);
    when(target.get(List.of(HANDLE_KEY, KEY_KEY))).thenAnswer(call -> {
      Map<String, String> pair = new HashMap<>();
      values.forEach((key, value) -> pair.put(key, value));
      return pair;
    });
    when(target.add(anyMap())).thenAnswer(call -> {
      values.putAll(call.<Map<String, String>>getArgument(0));
      return target;
    });
    doAnswer(call -> {
      values.remove(HANDLE_KEY);
      values.remove(KEY_KEY);
      return null;
    }).when(target).remove(HANDLE_KEY, KEY_KEY);

    return target;
  }

  private static String createName(Instant expiresAt, int index) {
    return "v1." + expiresAt.toEpochMilli() + "." + String.format("%043d", index);
  }

  private List<String> readLines() throws IOException {
    return Files.readAllLines(file);
  }

  private List<String> readNames() throws IOException {
    return readLines().stream().map(line -> line.substring(0, line.indexOf('\t'))).toList();
  }

  @Nested
  class SaveSession {

    @Test
    void shouldKeepTheSessionInTheFileAndThePairInTheTab() throws IOException {
      store.saveSession(session);

      List<String> lines = readLines();
      assertEquals(1, lines.size());
      String name = tab.get(HANDLE_KEY);
      assertTrue(lines.get(0).startsWith(name + "\t"));
      assertEquals("v1." + EXPIRES_AT.toEpochMilli() + ".", name.substring(0, name.length() - 43));
      assertFalse(lines.get(0).contains(TOKEN));
      assertFalse(tab.get(KEY_KEY).isBlank());
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
    void shouldReplaceThePreviousEntryOfTheTab() throws IOException {
      store.saveSession(session);
      String first = tab.get(HANDLE_KEY);

      store.saveSession(session);

      assertEquals(1, readLines().size());
      assertNotEquals(first, tab.get(HANDLE_KEY));
      assertEquals(List.of(tab.get(HANDLE_KEY)), readNames());
    }

    @Test
    void shouldKeepTheEntryOfAnotherTab() throws IOException {
      Map<String, String> otherTab = new HashMap<>();
      createStore(createStorage(otherTab), NOW).saveSession(session);

      store.saveSession(session);

      assertEquals(Set.of(tab.get(HANDLE_KEY), otherTab.get(HANDLE_KEY)), Set.copyOf(readNames()));
    }

    @Test
    void shouldDropExpiredEntriesAndLinesThatAreNoEntries() throws IOException {
      String live = createName(EXPIRES_AT, 1);
      Files.createDirectories(file.getParent());
      Files.write(file,
          List.of(createName(NOW, 2) + "\tAAAA", createName(NOW.minusSeconds(1), 3) + "\tAAAA",
              live + "\tAAAA", "no entry", "v1.1.short\tAAAA", createName(EXPIRES_AT, 4)));

      store.saveSession(session);

      assertEquals(Set.of(live, tab.get(HANDLE_KEY)), Set.copyOf(readNames()));
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
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
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
      assertTrue(names.contains(tab.get(HANDLE_KEY)));
      assertEquals(
          List.of("The session store removed 1 sessions because its limit of 10000 was reached"),
          messages);
    }

    @Test
    void shouldFailWithoutStorage() {
      BBjFileSessionStore withoutStorage = new BBjFileSessionStore(() -> directory, () -> APP, () -> null);

      IllegalStateException thrown = assertThrows(IllegalStateException.class,
          () -> withoutStorage.saveSession(session));

      assertEquals("The sign in could not be kept", thrown.getMessage());
    }

    @Test
    void shouldFailWhenTheFolderCannotBeCreated() throws IOException {
      Path blocker = Files.createFile(directory.resolve("blocker"));
      BBjFileSessionStore blocked = new BBjFileSessionStore(() -> blocker, () -> APP, () -> storage);

      IllegalStateException thrown = assertThrows(IllegalStateException.class, () -> blocked.saveSession(session));

      assertEquals("The sign in could not be kept", thrown.getMessage());
      assertTrue(tab.isEmpty());
    }

    @Test
    void shouldFailWhenTheDirectoryCannotBeResolved() {
      BBjFileSessionStore broken = new BBjFileSessionStore(() -> {
        throw new IllegalArgumentException("secret-path");
      }, () -> APP, () -> storage);

      IllegalStateException thrown = assertThrows(IllegalStateException.class, () -> broken.saveSession(session));

      assertEquals("The sign in could not be kept", thrown.getMessage());
    }

    @Test
    void shouldFailWhenTheSessionCannotBeEncrypted() {
      BBjSession large = new BBjSession(session.getPrincipal(), "t".repeat(70000), EXPIRES_AT);

      IllegalStateException thrown = assertThrows(IllegalStateException.class, () -> store.saveSession(large));

      assertEquals("The sign in could not be encrypted", thrown.getMessage());
      assertTrue(tab.isEmpty());
    }

    @Test
    void shouldRestoreThePreviousPairAndEntryWhenTheTabCannotBeWritten() throws IOException {
      store.saveSession(session);
      Map<String, String> previous = Map.copyOf(tab);
      SessionStorage failing = createStorage(tab);
      doThrow(new IllegalStateException("denied")).doAnswer(call -> {
        tab.putAll(call.<Map<String, String>>getArgument(0));
        return failing;
      }).when(failing).add(anyMap());
      BBjFileSessionStore failingStore = createStore(failing, NOW);

      assertThrows(IllegalStateException.class, () -> failingStore.saveSession(session));

      assertEquals(previous, tab);
      assertEquals(List.of(previous.get(HANDLE_KEY)), readNames());
    }

    @Test
    void shouldLeaveTheTabEmptyWhenItHoldsNoPreviousPairAndCannotBeWritten() throws IOException {
      SessionStorage failing = createStorage(tab);
      doThrow(new IllegalStateException("denied")).when(failing).add(anyMap());
      BBjFileSessionStore failingStore = createStore(failing, NOW);

      assertThrows(IllegalStateException.class, () -> failingStore.saveSession(session));

      assertTrue(tab.isEmpty());
      assertTrue(readLines().isEmpty());
    }

    @Test
    void shouldStillSaveWhenTheSweepFails() throws IOException {
      Clock failing = mock(Clock.class);
      when(failing.instant()).thenThrow(new IllegalStateException(TOKEN));
      BBjFileSessionStore failingStore = new BBjFileSessionStore(() -> directory, () -> APP, () -> storage, failing);
      List<String> messages = new ArrayList<>();
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
      Handler handler = createHandler(messages);
      logger.addHandler(handler);

      try {
        failingStore.saveSession(session);
      } finally {
        logger.removeHandler(handler);
      }

      assertEquals(List.of(tab.get(HANDLE_KEY)), readNames());
      assertEquals(
          List.of("Expired sign ins could not be removed (java.lang.IllegalStateException)"),
          messages);
    }

    @Test
    void shouldAcceptTheConstructorWithTheSystemClock() {
      BBjSession later = new BBjSession(session.getPrincipal(), TOKEN, Instant.now().plusSeconds(3600));
      BBjFileSessionStore system = new BBjFileSessionStore(() -> directory, () -> APP, () -> storage);

      system.saveSession(later);

      assertEquals(TOKEN, system.readSession().orElseThrow().getToken());
    }
  }

  @Nested
  class ReadSession {

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
      BBjFileSessionStore withoutStorage = new BBjFileSessionStore(() -> directory, () -> APP, () -> null);

      assertTrue(withoutStorage.readSession().isEmpty());
    }

    @Test
    void shouldReadNothingAndTouchNothingWhenTheTabHoldsNoPair() {
      assertTrue(store.readSession().isEmpty());
      assertFalse(Files.exists(file));
    }

    @Test
    void shouldRemoveAnExpiredEntryWithoutReadingIt() throws IOException {
      store.saveSession(session);
      BBjFileSessionStore later = createStore(storage, EXPIRES_AT);

      assertTrue(later.readSession().isEmpty());

      assertTrue(tab.isEmpty());
      assertTrue(readLines().isEmpty());
    }

    @Test
    void shouldRemoveThePairWhenTheEntryIsGone() throws IOException {
      store.saveSession(session);
      Files.write(file, List.of());

      assertTrue(store.readSession().isEmpty());

      assertTrue(tab.isEmpty());
    }

    @Test
    void shouldRemoveThePairWhenTheFileIsMissing() throws IOException {
      store.saveSession(session);
      Files.delete(file);

      assertTrue(store.readSession().isEmpty());

      assertTrue(tab.isEmpty());
      assertFalse(Files.exists(file));
    }

    @ParameterizedTest
    @MethodSource("unusablePairs")
    void shouldRemovePairThatIsNotUsable(Map<String, String> pair) {
      tab.putAll(pair);

      assertTrue(store.readSession().isEmpty());

      assertTrue(tab.isEmpty());
    }

    @Test
    void shouldRemoveTheEntryAndWarnWhenTheKeyOfTheTabWasChanged() throws IOException {
      store.saveSession(session);
      tab.put(KEY_KEY, "A".repeat(43));
      List<String> messages = new ArrayList<>();
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
      Handler handler = createHandler(messages);
      logger.addHandler(handler);

      try {
        assertTrue(store.readSession().isEmpty());
      } finally {
        logger.removeHandler(handler);
      }

      assertTrue(tab.isEmpty());
      assertTrue(readLines().isEmpty());
      assertEquals(List.of("A stored sign in could not be read and was removed"), messages);
    }

    @Test
    void shouldRemoveTheEntryWhenItsValueWasChanged() throws IOException {
      store.saveSession(session);
      String line = readLines().get(0);
      int split = line.indexOf('\t') + 1;
      char first = line.charAt(split);
      Files.write(file, List
          .of(line.substring(0, split) + (first == 'A' ? 'B' : 'A') + line.substring(split + 1)));

      assertTrue(store.readSession().isEmpty());

      assertTrue(tab.isEmpty());
      assertTrue(readLines().isEmpty());
    }

    @Test
    void shouldLogTheClassNameAndKeepTheTabWhenTheTabCannotBeRead() {
      SessionStorage failing = mock(SessionStorage.class);
      when(failing.get(List.of(HANDLE_KEY, KEY_KEY))).thenThrow(new IllegalStateException(TOKEN));
      BBjFileSessionStore failingStore = createStore(failing, NOW);
      List<String> messages = new ArrayList<>();
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
      Handler handler = createHandler(messages);
      logger.addHandler(handler);

      try {
        assertTrue(failingStore.readSession().isEmpty());
      } finally {
        logger.removeHandler(handler);
      }

      assertEquals(
          List.of("The stored sign in could not be read (java.lang.IllegalStateException)"),
          messages);
    }

    private static Stream<Arguments> unusablePairs() {
      return Stream.of(Arguments.of(Map.of(HANDLE_KEY, createName(EXPIRES_AT, 1))),
          Arguments.of(Map.of(HANDLE_KEY, "not an entry", KEY_KEY, "A".repeat(43))),
          Arguments.of(Map.of(HANDLE_KEY, createName(NOW, 1), KEY_KEY, "A".repeat(43))));
    }
  }

  @Nested
  class ClearSession {

    @Test
    void shouldRemoveTheEntryOfTheTabAndKeepTheOthers() throws IOException {
      Map<String, String> otherTab = new HashMap<>();
      createStore(createStorage(otherTab), NOW).saveSession(session);
      store.saveSession(session);

      store.clearSession();

      assertTrue(tab.isEmpty());
      assertEquals(List.of(otherTab.get(HANDLE_KEY)), readNames());
    }

    @Test
    void shouldNotCreateTheFileWhenThereIsNone() {
      tab.put(HANDLE_KEY, createName(EXPIRES_AT, 1));

      store.clearSession();

      assertTrue(tab.isEmpty());
      assertFalse(Files.exists(file));
    }

    @Test
    void shouldLeaveTheFileAloneForNameThatIsNoEntry() throws IOException {
      store.saveSession(session);
      tab.put(HANDLE_KEY, "not an entry");
      List<String> before = readLines();

      store.clearSession();

      assertTrue(tab.isEmpty());
      assertEquals(before, readLines());
    }

    @Test
    void shouldDoNothingWithoutStorage() {
      BBjFileSessionStore withoutStorage = new BBjFileSessionStore(() -> directory, () -> APP, () -> null);

      assertDoesNotThrow(withoutStorage::clearSession);
    }

    @Test
    void shouldLogTheClassNameWhenTheStorageFails() throws IOException {
      store.saveSession(session);
      SessionStorage failing = createStorage(tab);
      doThrow(new IllegalStateException(TOKEN)).when(failing).remove(HANDLE_KEY, KEY_KEY);
      BBjFileSessionStore failingStore = createStore(failing, NOW);
      List<String> messages = new ArrayList<>();
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
      Handler handler = createHandler(messages);
      logger.addHandler(handler);

      try {
        failingStore.clearSession();
      } finally {
        logger.removeHandler(handler);
      }

      assertEquals(
          List.of(
              "The sign in could not be removed from the tab (java.lang.IllegalStateException)"),
          messages);
      assertTrue(readLines().isEmpty());
    }

    @Test
    void shouldNotThrowWhenTheStorageCannotBeResolved() {
      BBjFileSessionStore failingStore = new BBjFileSessionStore(() -> directory, () -> APP, () -> {
        throw new IllegalStateException(TOKEN);
      });

      assertDoesNotThrow(failingStore::clearSession);
    }

    @Test
    void shouldNotThrowWhenTheTabCannotBeRead() {
      SessionStorage failing = mock(SessionStorage.class);
      when(failing.get(List.of(HANDLE_KEY, KEY_KEY))).thenThrow(new IllegalStateException(TOKEN));
      BBjFileSessionStore failingStore = createStore(failing, NOW);

      assertDoesNotThrow(failingStore::clearSession);
    }

    @Test
    void shouldLogTheClassNameWhenTheEntryCannotBeRemoved() throws IOException {
      tab.put(HANDLE_KEY, createName(EXPIRES_AT, 1));
      Files.createDirectories(file);
      List<String> messages = new ArrayList<>();
      Logger logger = Logger.getLogger(BBjFileSessionStore.class.getName());
      Handler handler = createHandler(messages);
      logger.addHandler(handler);

      try {
        store.clearSession();
      } finally {
        logger.removeHandler(handler);
      }

      assertTrue(tab.isEmpty());
      assertEquals(1, messages.size());
      assertTrue(messages.get(0).startsWith("The session entry could not be removed ("));
    }
  }

  @Nested
  class CreateApplicationKey {

    @ParameterizedTest
    @MethodSource("applicationNames")
    void shouldCreateFileNameSafeKey(String applicationName, String expected) {
      assertEquals(expected, BBjFileSessionStore.createApplicationKey(applicationName));
    }

    private static Stream<Arguments> applicationNames() {
      return Stream.of(Arguments.of("shop", "shop"), Arguments.of("My_app-2", "My_app-2"),
          Arguments.of("my app", "my_app-cccdfa68"), Arguments.of("", "app-e3b0c442"),
          Arguments.of("x".repeat(70), "x".repeat(64) + "-c71bd109"));
    }
  }

  private static Handler createHandler(List<String> messages) {
    return new Handler() {
      @Override
      public void publish(LogRecord logRecord) {
        messages.add(new SimpleFormatter().formatMessage(logRecord));
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
  }
}
