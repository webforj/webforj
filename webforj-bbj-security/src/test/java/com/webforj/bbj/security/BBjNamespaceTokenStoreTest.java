package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.webforj.bbj.security.fixture.CapturedLog;
import com.webforj.bbj.security.fixture.InMemoryNamespace;
import com.webforj.bbj.security.fixture.InMemorySessionStorage;
import com.webforj.environment.namespace.GlobalNamespace;
import com.webforj.environment.namespace.exception.NamespaceLockedException;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.webstorage.SessionStorage;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjNamespaceTokenStoreTest {
  private static final String HANDLE_KEY = "webforj-bbj-handle";
  private static final String KEY_KEY = "webforj-bbj-key";
  private static final String TOKEN = "eyJhbGciOiJIUzI1NiJ9.secretPayload.secretSignature";
  private static final Instant NOW = Instant.parse("2026-10-22T10:00:00Z");
  private static final Clock CLOCK = Clock.fixed(NOW, ZoneOffset.UTC);
  private static final String APPLICATION = "test-app";
  private static final String PREFIX = "webforj.bbj-security-tokens.test-app.";
  private static final Pattern ENTRY_NAME =
      Pattern.compile("webforj\\.bbj-security-tokens\\.test-app\\.v1\\.\\d+\\.[A-Za-z0-9_-]{43}");
  private static final String UNREADABLE = "A stored sign-in could not be read and was removed";
  private final InMemoryNamespace namespace = new InMemoryNamespace();
  private final InMemorySessionStorage storage = new InMemorySessionStorage();
  private final BBjLoginSession session =
      new BBjLoginSession("alice", TOKEN, NOW.plus(Duration.ofMinutes(30)));
  private BBjNamespaceTokenStore store;
  private CapturedLog log;

  @BeforeEach
  void setUp() {
    log = new CapturedLog(BBjNamespaceTokenStore.class.getName());
    store = new BBjNamespaceTokenStore(namespace::getNamespace, () -> APPLICATION,
        storage::getStorage, CLOCK);
  }

  @AfterEach
  void tearDown() {
    log.close();
  }

  private static String createName(Instant expiresAt, char fill) {
    return PREFIX + "v1." + expiresAt.toEpochMilli() + "." + String.valueOf(fill).repeat(43);
  }

  private static String createName(Instant expiresAt, int index) {
    return createName(PREFIX, expiresAt, index);
  }

  private static String createName(String prefix, Instant expiresAt, int index) {
    return prefix + "v1." + expiresAt.toEpochMilli() + "." + "%043d".formatted(index);
  }

  private BBjNamespaceTokenStore createStore(String application, InMemorySessionStorage tab) {
    return new BBjNamespaceTokenStore(namespace::getNamespace, () -> application, tab::getStorage,
        CLOCK);
  }

  private String getStoredName() {
    return storage.getValues().get(HANDLE_KEY);
  }

  private boolean hasSecretInLog() {
    String key = storage.getValues().get(KEY_KEY);

    return log.getAllMessages().stream().anyMatch(text -> text.contains(TOKEN)
        || text.contains("alice") || (key != null && text.contains(key)));
  }

  @Nested
  class SaveSession {
    @Test
    void shouldKeepOneEncryptedEntryAndTheTwoStorageKeys() {
      store.saveSession(session);

      assertEquals(Set.of(HANDLE_KEY, KEY_KEY), storage.getValues().keySet());
      assertEquals(1, namespace.getValues().size());
      String name = namespace.getValues().keySet().iterator().next();
      assertEquals(name, getStoredName());
      assertTrue(ENTRY_NAME.matcher(name).matches());
      assertEquals(String.valueOf(session.getExpiresAt().toEpochMilli()),
          name.substring(PREFIX.length()).split("\\.")[1]);
      assertTrue(namespace.getValues().get(name) instanceof String);
      String everything = namespace.getValues() + " " + storage.getValues();
      assertFalse(everything.contains(TOKEN) || everything.contains("alice"));
      assertFalse(everything.contains("eyJ"));
    }

    @Test
    void shouldReplaceThePreviousEntryWithNewHandleAndKey() {
      store.saveSession(session);
      String previousName = getStoredName();
      final String previousKey = storage.getValues().get(KEY_KEY);

      store.saveSession(session);

      assertEquals(1, namespace.getValues().size());
      assertFalse(namespace.getValues().containsKey(previousName));
      assertNotEquals(previousName, getStoredName());
      assertNotEquals(previousKey, storage.getValues().get(KEY_KEY));
      assertEquals(session.getUserName(), store.readSession().orElseThrow().getUserName());
    }

    @Test
    void shouldNotChangeAnythingWhenTheSessionCannotBeEncrypted() {
      store.saveSession(session);
      final Map<String, String> pair = Map.copyOf(storage.getValues());
      final Map<String, Object> entries = Map.copyOf(namespace.getValues());
      BBjLoginSession tooLong = new BBjLoginSession("a".repeat(70000), TOKEN, NOW.plusSeconds(60));

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(tooLong));

      assertNull(thrown.getCause());
      assertEquals(pair, storage.getValues());
      assertEquals(entries, namespace.getValues());
    }

    @Test
    void shouldKeepThePreviousSignInWhenTheEntryCannotBePut() throws Exception {
      store.saveSession(session);
      final Map<String, String> pair = Map.copyOf(storage.getValues());
      final Map<String, Object> entries = Map.copyOf(namespace.getValues());
      doThrow(new NamespaceLockedException("locked " + TOKEN)).when(namespace.getNamespace())
          .put(anyString(), any());

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(session));

      assertNull(thrown.getCause());
      assertFalse(thrown.getMessage().contains(TOKEN));
      assertEquals(pair, storage.getValues());
      assertEquals(entries, namespace.getValues());
      assertFalse(hasSecretInLog());
      assertTrue(log.getAllMessages().stream()
          .anyMatch(text -> text.contains(NamespaceLockedException.class.getName())));
    }

    @Test
    void shouldRestoreThePreviousPairWhenTheStorageWriteFails() {
      store.saveSession(session);
      final Map<String, String> pair = Map.copyOf(storage.getValues());
      storage.setFailingWrites(1);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(session));

      assertNull(thrown.getCause());
      assertEquals(pair, storage.getValues());
      assertEquals(Set.of(pair.get(HANDLE_KEY)), namespace.getValues().keySet());
      assertTrue(store.readSession().isPresent());
      assertFalse(storage.getRejectedValues().isEmpty());
      assertFalse(hasRejectedValue(thrown.getMessage()));
      assertFalse(hasRejectedValue(String.join(" ", log.getAllMessages())));
    }

    @Test
    void shouldRemoveBothKeysWhenThePreviousPairCannotBeRestored() {
      store.saveSession(session);
      final String previousName = getStoredName();
      storage.setFailingWrites(2);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(session));

      assertNull(thrown.getCause());
      assertTrue(storage.getValues().isEmpty());
      assertEquals(Set.of(previousName), namespace.getValues().keySet());
      assertFalse(storage.getRejectedValues().isEmpty());
      assertFalse(hasRejectedValue(thrown.getMessage()));
      assertFalse(hasRejectedValue(String.join(" ", log.getAllMessages())));
    }

    @Test
    void shouldRemoveTheNewEntryWhenTheFirstWriteToTheStorageFails() {
      storage.setFailingWrites(1);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(session));

      assertNull(thrown.getCause());
      assertTrue(storage.getValues().isEmpty());
      assertTrue(namespace.getValues().isEmpty());
    }

    @Test
    void shouldSucceedAndWarnWhenThePreviousEntryCannotBeRemoved() throws Exception {
      store.saveSession(session);
      final String previousName = getStoredName();
      doThrow(new NamespaceLockedException()).when(namespace.getNamespace()).remove(previousName);

      assertDoesNotThrow(() -> store.saveSession(session));

      assertNotEquals(previousName, getStoredName());
      assertTrue(namespace.getValues().containsKey(previousName));
      assertEquals(List.of("The previous sign-in entry could not be removed"),
          log.getMessages(Level.WARNING));
      assertTrue(store.readSession().isPresent());
    }

    @Test
    void shouldThrowWithoutCauseWhenTheNamespaceCannotBeOpened() {
      Supplier<GlobalNamespace> failing = () -> {
        throw new WebforjRuntimeException("secret detail " + TOKEN);
      };
      BBjNamespaceTokenStore unavailable =
          new BBjNamespaceTokenStore(failing, () -> APPLICATION, storage::getStorage, CLOCK);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> unavailable.saveSession(session));

      assertNull(thrown.getCause());
      assertTrue(storage.getValues().isEmpty());
      assertFalse(hasSecretInLog());
    }

    @Test
    void shouldThrowWhenThereIsNoSessionStorage() {
      Supplier<SessionStorage> none = () -> null;
      BBjNamespaceTokenStore withoutStorage =
          new BBjNamespaceTokenStore(namespace::getNamespace, () -> APPLICATION, none, CLOCK);

      assertThrows(IllegalStateException.class, () -> withoutStorage.saveSession(session));

      assertTrue(namespace.getValues().isEmpty());
    }

    private boolean hasRejectedValue(String text) {
      return storage.getRejectedValues().stream()
          .anyMatch(rejected -> rejected.values().stream().anyMatch(text::contains));
    }
  }

  @Nested
  class ReadSession {
    static Stream<Arguments> unusableKeys() {
      return Stream.of(Arguments.of(Map.of(HANDLE_KEY, "v1.1.x")),
          Arguments.of(Map.of(KEY_KEY, "someKey")));
    }

    static Stream<Instant> expiries() {
      return Stream.of(NOW, NOW.minusSeconds(3600));
    }

    @Test
    void shouldGiveTheSavedSession() {
      store.saveSession(session);

      BBjLoginSession read = store.readSession().orElseThrow();

      assertEquals("alice", read.getUserName());
      assertEquals(TOKEN, read.getToken());
      assertEquals(session.getExpiresAt(), read.getExpiresAt());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldAnswerEmptyWithoutLoggingWhenNothingIsKept() {
      assertEquals(Optional.empty(), store.readSession());

      assertTrue(log.getAllMessages().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("unusableKeys")
    void shouldRemoveTheKeysWhenOnlyOneOfThemIsKept(Map<String, String> kept) {
      storage.getValues().putAll(kept);

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("expiries")
    void shouldClearAnExpiredEntryWithoutReadingIt(Instant expiresAt) {
      String name = createName(expiresAt, 'e');
      namespace.getValues().put(name, "not even encrypted");
      storage.getValues().putAll(Map.of(HANDLE_KEY, name, KEY_KEY, "someKey"));

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(namespace.getValues().isEmpty());
      assertTrue(storage.getValues().isEmpty());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldClearAnEntryNameThatIsNotOfThisFormat() {
      storage.getValues().putAll(Map.of(HANDLE_KEY, "garbage", KEY_KEY, "someKey"));

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
    }

    @Test
    void shouldClearWhenTheEntryIsMissing() {
      store.saveSession(session);
      namespace.getValues().clear();

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldClearWhenTheValueIsNotText() {
      store.saveSession(session);
      namespace.getValues().put(getStoredName(), 42);

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
      assertTrue(namespace.getValues().isEmpty());
    }

    @Test
    void shouldClearAndWarnOnceWhenTheEntryWasChanged() {
      store.saveSession(session);
      String name = getStoredName();
      String value = (String) namespace.getValues().get(name);
      namespace.getValues().put(name, value.substring(0, 30) + "AAAA" + value.substring(34));

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
      assertTrue(namespace.getValues().isEmpty());
      assertEquals(List.of(UNREADABLE), log.getMessages(Level.WARNING));
    }

    @Test
    void shouldClearAndWarnOnceWhenTheKeyIsNotTheKeyOfTheEntry() {
      store.saveSession(session);
      storage.getValues().put(KEY_KEY, new BBjTokenEntryCipher().createKey());

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
      assertTrue(namespace.getValues().isEmpty());
      assertEquals(List.of(UNREADABLE), log.getMessages(Level.WARNING));
    }

    @Test
    void shouldAnswerEmptyAndKeepEverythingWhenTheNamespaceFails() {
      store.saveSession(session);
      final Map<String, String> pair = Map.copyOf(storage.getValues());
      Supplier<GlobalNamespace> failing = () -> {
        throw new WebforjRuntimeException("secret detail " + TOKEN);
      };
      BBjNamespaceTokenStore unavailable =
          new BBjNamespaceTokenStore(failing, () -> APPLICATION, storage::getStorage, CLOCK);

      assertEquals(Optional.empty(), unavailable.readSession());

      assertEquals(pair, storage.getValues());
      assertEquals(1, log.getMessages(Level.WARNING).size());
      assertFalse(hasSecretInLog());
      assertTrue(log.getAllMessages().get(0).contains(WebforjRuntimeException.class.getName()));
    }

    @Test
    void shouldAnswerEmptyWhenThereIsNoSessionStorage() {
      Supplier<SessionStorage> none = () -> null;
      BBjNamespaceTokenStore withoutStorage =
          new BBjNamespaceTokenStore(namespace::getNamespace, () -> APPLICATION, none, CLOCK);

      assertEquals(Optional.empty(), withoutStorage.readSession());
    }
  }

  @Nested
  class ClearSession {
    @Test
    void shouldRemoveTheEntryAndBothKeys() {
      store.saveSession(session);

      store.clearSession();

      assertTrue(namespace.getValues().isEmpty());
      assertTrue(storage.getValues().isEmpty());
    }

    @Test
    void shouldRemoveTheKeysWhenTheEntryCannotBeRemoved() throws Exception {
      store.saveSession(session);
      doThrow(new NamespaceLockedException()).when(namespace.getNamespace()).remove(anyString());

      assertDoesNotThrow(store::clearSession);

      assertTrue(storage.getValues().isEmpty());
      assertEquals(1, namespace.getValues().size());
      assertEquals(List.of("The sign-in entry could not be removed"),
          log.getMessages(Level.WARNING));
    }

    @Test
    void shouldRemoveTheEntryWhenTheKeysCannotBeRemoved() {
      store.saveSession(session);
      doThrow(new WebforjRuntimeException("secret " + TOKEN)).when(storage.getStorage())
          .remove(any(String[].class));

      assertDoesNotThrow(store::clearSession);

      assertTrue(namespace.getValues().isEmpty());
      assertEquals(2, storage.getValues().size());
      assertFalse(hasSecretInLog());
    }

    @Test
    void shouldDoNothingWhenNothingIsKept() {
      assertDoesNotThrow(store::clearSession);

      assertTrue(log.getAllMessages().isEmpty());
    }

    @Test
    void shouldNotThrowWhenThereIsNoSessionStorage() {
      Supplier<SessionStorage> none = () -> null;
      BBjNamespaceTokenStore withoutStorage =
          new BBjNamespaceTokenStore(namespace::getNamespace, () -> APPLICATION, none, CLOCK);

      assertDoesNotThrow(withoutStorage::clearSession);
    }
  }

  @Nested
  class Sweep {
    @Test
    void shouldRemoveExpiredEntriesAndKeepForeignNamesAndOtherVersions() {
      String expired = createName(NOW.minusSeconds(1), 'a');
      String atExpiry = createName(NOW, 'b');
      String live = createName(NOW.plusSeconds(60), 'c');
      String shortHandle = PREFIX + "v1." + NOW.minusSeconds(5).toEpochMilli() + ".short";
      String otherVersion =
          PREFIX + "v2." + NOW.minusSeconds(5).toEpochMilli() + "." + "d".repeat(43);
      for (String name : List.of(expired, atExpiry, live, shortHandle, otherVersion, "foreign")) {
        namespace.getValues().put(name, "value");
      }

      store.saveSession(session);

      assertEquals(Set.of(live, shortHandle, otherVersion, "foreign", getStoredName()),
          namespace.getValues().keySet());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldEvictTheEarliestExpiryAndWarnOnceWithTheCountAtTheLimitOfTenThousand() {
      List<String> names = IntStream.range(0, 10002)
          .mapToObj(index -> createName(NOW.plusSeconds(60 + index), index)).toList();
      names.forEach(name -> namespace.getValues().put(name, "value"));

      store.saveSession(session);

      assertEquals(10000, namespace.getValues().size());
      Set<String> expected = new HashSet<>(names.subList(3, 10002));
      expected.add(getStoredName());
      assertEquals(expected, namespace.getValues().keySet());
      assertEquals(
          List.of("The token store removed 3 sign-ins because its limit of 10000 was reached"),
          log.getMessages(Level.WARNING));
    }

    @Test
    void shouldContinueWhenOneRemovalFails() throws Exception {
      String first = createName(NOW.minusSeconds(10), 'a');
      String second = createName(NOW.minusSeconds(5), 'b');
      namespace.getValues().put(first, "value");
      namespace.getValues().put(second, "value");
      doThrow(new NamespaceLockedException()).when(namespace.getNamespace()).remove(first);

      assertDoesNotThrow(() -> store.saveSession(session));

      assertEquals(Set.of(first, getStoredName()), namespace.getValues().keySet());
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldSaveAndWarnWhenTheKeysCannotBeListed() {
      doThrow(new WebforjRuntimeException("listing failed")).when(namespace.getNamespace())
          .keySet();

      assertDoesNotThrow(() -> store.saveSession(session));

      assertEquals(1, namespace.getValues().size());
      assertEquals(List.of("Expired sign-ins could not be removed ("
          + WebforjRuntimeException.class.getName() + ")"), log.getMessages(Level.WARNING));
    }
  }

  @Nested
  class CreateApplicationKey {
    @Test
    void shouldKeepAnApplicationNameThatIsSafe() {
      assertEquals("security-integration",
          BBjNamespaceTokenStore.createApplicationKey("security-integration"));
    }

    @Test
    void shouldReplaceUnsafeCharactersAndAddTheHashOfTheOriginalName() {
      String first = BBjNamespaceTokenStore.createApplicationKey("my app");
      String second = BBjNamespaceTokenStore.createApplicationKey("my_app");

      assertTrue(first.matches("my_app-[0-9a-f]{8}"), first);
      assertEquals("my_app", second);
      assertEquals(first, BBjNamespaceTokenStore.createApplicationKey("my app"));
    }

    @Test
    void shouldNeverHoldADotSoThatNoKeyPrefixOfOneApplicationFitsAnother() {
      assertFalse(BBjNamespaceTokenStore.createApplicationKey("my.app.v1").contains("."));
    }

    @Test
    void shouldShortenLongNamesAndKeepTheirHash() {
      String name = BBjNamespaceTokenStore.createApplicationKey("n".repeat(200));

      assertTrue(name.matches("n{64}-[0-9a-f]{8}"), name);
    }

    @Test
    void shouldUsePlaceholderForEmptyName() {
      assertTrue(BBjNamespaceTokenStore.createApplicationKey("").matches("app-[0-9a-f]{8}"));
    }
  }

  @Nested
  class Applications {
    private static final String OTHER_PREFIX = "webforj.bbj-security-tokens.other-app.";
    private final InMemorySessionStorage otherTab = new InMemorySessionStorage();
    private BBjNamespaceTokenStore other;

    @BeforeEach
    void setUp() {
      other = createStore("other-app", otherTab);
    }

    @Test
    void shouldKeepTheSignInsOfTwoApplicationsApart() {
      store.saveSession(session);
      other.saveSession(new BBjLoginSession("bob", TOKEN, session.getExpiresAt()));

      assertEquals(2, namespace.getValues().size());
      assertTrue(getStoredName().startsWith(PREFIX + "v1."));
      assertTrue(otherTab.getValues().get(HANDLE_KEY).startsWith(OTHER_PREFIX + "v1."));
      assertEquals("alice", store.readSession().orElseThrow().getUserName());
      assertEquals("bob", other.readSession().orElseThrow().getUserName());
    }

    @Test
    void shouldNotTouchEntriesOfAnotherApplicationWhenSweeping() {
      String expired = createName(OTHER_PREFIX, NOW.minusSeconds(60), 'x');
      String expiredOfLongerName =
          createName("webforj.bbj-security-tokens.test-app-two.", NOW.minusSeconds(60), 'y');
      namespace.getValues().put(expired, "value");
      namespace.getValues().put(expiredOfLongerName, "value");

      store.saveSession(session);

      assertTrue(namespace.getValues().containsKey(expired));
      assertTrue(namespace.getValues().containsKey(expiredOfLongerName));
      assertEquals(3, namespace.getValues().size());
    }

    @Test
    void shouldEvictOnlyEntriesOfItsOwnApplication() {
      List<String> names = IntStream.range(0, 10002)
          .mapToObj(index -> createName(NOW.plusSeconds(60 + index), index)).toList();
      List<String> others = IntStream.range(0, 5000)
          .mapToObj(index -> createName(OTHER_PREFIX, NOW.plusSeconds(60 + index), index)).toList();
      List<String> othersExpired = IntStream.range(5000, 10000)
          .mapToObj(index -> createName(OTHER_PREFIX, NOW.minusSeconds(60 + index), index))
          .toList();
      names.forEach(name -> namespace.getValues().put(name, "value"));
      others.forEach(name -> namespace.getValues().put(name, "value"));
      othersExpired.forEach(name -> namespace.getValues().put(name, "value"));

      store.saveSession(session);

      Set<String> expected = new HashSet<>(names.subList(3, 10002));
      expected.add(getStoredName());
      expected.addAll(others);
      expected.addAll(othersExpired);
      assertEquals(expected, namespace.getValues().keySet());
      assertEquals(
          List.of("The token store removed 3 sign-ins because its limit of 10000 was reached"),
          log.getMessages(Level.WARNING));
    }

    @Test
    void shouldAnswerEmptyAndKeepTheEntryWhenTheHandleBelongsToAnotherApplication() {
      other.saveSession(session);
      storage.getValues().putAll(otherTab.getValues());

      assertEquals(Optional.empty(), store.readSession());

      assertTrue(storage.getValues().isEmpty());
      assertEquals(1, namespace.getValues().size());
      assertTrue(other.readSession().isPresent());
    }

    @Test
    void shouldNotRemoveThePreviousEntryOfAnotherApplicationOnSave() {
      other.saveSession(session);
      storage.getValues().putAll(otherTab.getValues());
      String otherName = storage.getValues().get(HANDLE_KEY);

      store.saveSession(session);

      assertTrue(namespace.getValues().containsKey(otherName));
      assertTrue(getStoredName().startsWith(PREFIX));
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    @Test
    void shouldNotRemoveTheEntryOfAnotherApplicationOnClear() {
      other.saveSession(session);
      storage.getValues().putAll(otherTab.getValues());

      store.clearSession();

      assertTrue(storage.getValues().isEmpty());
      assertEquals(1, namespace.getValues().size());
      assertTrue(log.getAllMessages().isEmpty());
    }

    @Test
    void shouldNotOpenAnEntryMovedToAnotherApplication() {
      store.saveSession(session);
      String name = getStoredName();
      String moved = OTHER_PREFIX + name.substring(PREFIX.length());
      namespace.getValues().put(moved, namespace.getValues().get(name));
      otherTab.getValues()
          .putAll(Map.of(HANDLE_KEY, moved, KEY_KEY, storage.getValues().get(KEY_KEY)));

      assertEquals(Optional.empty(), other.readSession());

      assertTrue(otherTab.getValues().isEmpty());
      assertFalse(namespace.getValues().containsKey(moved));
      assertEquals(List.of(UNREADABLE), log.getMessages(Level.WARNING));
      assertTrue(store.readSession().isPresent());
    }

    @Test
    void shouldNeverClearTheNamespace() {
      List<String> names = IntStream.range(0, 10002)
          .mapToObj(index -> createName(NOW.plusSeconds(60 + index), index)).toList();
      names.forEach(name -> namespace.getValues().put(name, "value"));

      store.saveSession(session);
      store.readSession();
      store.saveSession(session);
      namespace.getValues().put(getStoredName(), 42);
      store.readSession();
      store.clearSession();

      verify(namespace.getNamespace(), never()).clear();
    }
  }

  @Test
  void shouldNeverLogTokenUserNameKeyOrHandleOverFullRun() {
    store.saveSession(session);
    String handle = getStoredName();
    String key = storage.getValues().get(KEY_KEY);
    store.readSession();
    store.saveSession(session);
    storage.setFailingWrites(2);
    assertThrows(IllegalStateException.class, () -> store.saveSession(session));
    namespace.getValues().clear();
    store.readSession();
    store.clearSession();

    for (String text : log.getAllMessages()) {
      assertFalse(text.contains(TOKEN) || text.contains("alice") || text.contains(key)
          || text.contains(handle), text);
    }
  }
}
