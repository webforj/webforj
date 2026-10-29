package com.webforj.bbj.security.store;

import static com.webforj.bbj.security.store.BrowserStoreFixture.createHandler;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.store.BBjEntryFile.Content;
import com.webforj.bbj.security.store.BBjThrottleStore.Kind;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Handler;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjThrottleStoreTest {
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  private static final Duration WINDOW = Duration.ofSeconds(60);
  private static final String LOGGER_NAME = BBjThrottleStore.class.getName();

  @TempDir
  private Path directory;

  private Clock clock;
  private BBjThrottleStore store;
  private Path file;

  @BeforeEach
  void setUp() {
    clock = mock(Clock.class);
    when(clock.instant()).thenReturn(NOW);
    store = createStore();
    file = directory.resolve("webforj-bbj-security").resolve("shop.counters");
  }

  @Test
  void shouldReadNoAttemptsWithoutCounter() {
    assertEquals(0, store.readAttempts(Kind.USER_NAME, "alice"));
  }

  @Test
  void shouldCountTheFailuresOfOneUserName() {
    assertEquals(1, store.addFailure(Kind.USER_NAME, "alice"));
    assertEquals(2, store.addFailure(Kind.USER_NAME, "alice"));
    assertEquals(3, store.addFailure(Kind.USER_NAME, "alice"));

    assertEquals(3, store.readAttempts(Kind.USER_NAME, "alice"));
    assertEquals(0, store.readAttempts(Kind.USER_NAME, "bob"));
  }

  @Test
  void shouldWriteOneEntryWithTheWindowAndNoUserName() throws IOException {
    store.addFailure(Kind.USER_NAME, "alice");
    when(clock.instant()).thenReturn(NOW.plusSeconds(10));
    store.addFailure(Kind.USER_NAME, "alice");

    List<String> lines = Files.readAllLines(file);
    assertEquals(1, lines.size());
    String expected = "t1\\.%d\\.u\\.[0-9a-f]{32}\\t2\\.%d"
        .formatted(NOW.plus(WINDOW).toEpochMilli(), NOW.toEpochMilli());
    assertTrue(Pattern.matches(expected, lines.get(0)), lines.get(0));
    assertFalse(lines.get(0).contains("alice"));
  }

  @Test
  void shouldKeepTheKindsApart() {
    store.addFailure(Kind.USER_NAME, "10.0.0.7");

    assertEquals(0, store.readAttempts(Kind.ADDRESS, "10.0.0.7"));
    assertEquals(1, store.readAttempts(Kind.USER_NAME, "10.0.0.7"));
  }

  @ParameterizedTest
  @MethodSource("sameValues")
  void shouldCountTheNormalFormOfTheValue(Kind kind, String counted, String read) {
    store.addFailure(kind, counted);

    assertEquals(1, store.readAttempts(kind, read));
  }

  private static Stream<Arguments> sameValues() {
    return Stream.of(Arguments.of(Kind.USER_NAME, " alice ", "alice"),
        Arguments.of(Kind.USER_NAME, "José", "José"),
        Arguments.of(Kind.ADDRESS, "FE80::1", " fe80::1 "));
  }

  @Test
  void shouldKeepTheCaseOfTheUserName() {
    store.addFailure(Kind.USER_NAME, "Alice");

    assertEquals(0, store.readAttempts(Kind.USER_NAME, "alice"));
  }

  @Test
  void shouldReadNoAttemptsOnceTheWindowHasEnded() {
    store.addFailure(Kind.USER_NAME, "alice");
    store.addFailure(Kind.USER_NAME, "alice");

    when(clock.instant()).thenReturn(NOW.plus(WINDOW).minusMillis(1));
    assertEquals(2, store.readAttempts(Kind.USER_NAME, "alice"));

    when(clock.instant()).thenReturn(NOW.plus(WINDOW));
    assertEquals(0, store.readAttempts(Kind.USER_NAME, "alice"));
  }

  @Test
  void shouldStartNewWindowWithTheFirstFailureAfterTheWindow() throws IOException {
    store.addFailure(Kind.USER_NAME, "alice");
    store.addFailure(Kind.USER_NAME, "alice");
    Instant later = NOW.plus(WINDOW).plusSeconds(5);
    when(clock.instant()).thenReturn(later);

    int attempts = store.addFailure(Kind.USER_NAME, "alice");

    assertEquals(1, attempts);
    List<String> lines = Files.readAllLines(file);
    assertEquals(1, lines.size());
    assertTrue(lines.get(0).startsWith("t1." + later.plus(WINDOW).toEpochMilli() + ".u."));
    assertTrue(lines.get(0).endsWith("\t1." + later.toEpochMilli()));
  }

  @Test
  void shouldStartNewWindowWhenTheEntryOutlivesItsWindow() throws IOException {
    BBjThrottleStore longer =
        new BBjThrottleStore(new BBjEntryFile(() -> directory, () -> "shop", Content.COUNTERS),
            WINDOW.multipliedBy(2), clock);
    longer.addFailure(Kind.USER_NAME, "alice");
    when(clock.instant()).thenReturn(NOW.plus(WINDOW));

    int attempts = store.addFailure(Kind.USER_NAME, "alice");

    assertEquals(1, attempts);
    assertEquals(1, Files.readAllLines(file).size());
  }

  @Test
  void shouldClearTheCounterOfOneValueOnly() {
    store.addFailure(Kind.USER_NAME, "alice");
    store.addFailure(Kind.USER_NAME, "bob");
    store.addFailure(Kind.ADDRESS, "10.0.0.7");

    store.clearAttempts(Kind.USER_NAME, "alice");

    assertEquals(0, store.readAttempts(Kind.USER_NAME, "alice"));
    assertEquals(1, store.readAttempts(Kind.USER_NAME, "bob"));
    assertEquals(1, store.readAttempts(Kind.ADDRESS, "10.0.0.7"));
  }

  @Test
  void shouldShareTheCountersBetweenStoresOfOneApplication() {
    BBjThrottleStore other = createStore();

    store.addFailure(Kind.USER_NAME, "alice");
    other.addFailure(Kind.USER_NAME, "alice");

    assertEquals(2, store.readAttempts(Kind.USER_NAME, "alice"));
  }

  @Test
  void shouldRemoveTheEarliestWindowEndAtTheLimitAndWarn() throws IOException {
    List<String> lines = new ArrayList<>();
    for (int index = 0; index < 10000; index++) {
      lines.add("t1." + NOW.plusSeconds(index + 1L).toEpochMilli() + ".a."
          + "%032x".formatted(index) + "\t1." + NOW.toEpochMilli());
    }
    Files.createDirectories(file.getParent());
    Files.write(file, lines);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    try {
      store.addFailure(Kind.USER_NAME, "alice");
    } finally {
      logger.removeHandler(handler);
    }

    List<String> written = Files.readAllLines(file);
    assertEquals(10000, written.size());
    assertFalse(written.contains(lines.get(0)));
    assertEquals(1, store.readAttempts(Kind.USER_NAME, "alice"));
    assertEquals(
        List.of("The throttle store removed 1 counters because its limit of 10000 was reached"),
        messages);
  }

  @ParameterizedTest
  @MethodSource("failingCalls")
  void shouldThrowAndLogTheClassNameWhenTheFileCannotBeUsed(Consumer<BBjThrottleStore> call,
      String expected) throws IOException {
    Files.createDirectories(file);
    List<String> messages = new ArrayList<>();
    Logger logger = Logger.getLogger(LOGGER_NAME);
    Handler handler = createHandler(messages);
    logger.addHandler(handler);

    IllegalStateException thrown;
    try {
      thrown = assertThrows(IllegalStateException.class, () -> call.accept(store));
    } finally {
      logger.removeHandler(handler);
    }

    assertEquals("The sign in counters could not be used", thrown.getMessage());
    assertEquals(1, messages.size());
    assertTrue(messages.get(0).startsWith(expected + " (java."), messages.get(0));
  }

  private static Stream<Arguments> failingCalls() {
    Consumer<BBjThrottleStore> read = target -> target.readAttempts(Kind.USER_NAME, "alice");
    Consumer<BBjThrottleStore> add = target -> target.addFailure(Kind.USER_NAME, "alice");
    Consumer<BBjThrottleStore> clear = target -> target.clearAttempts(Kind.USER_NAME, "alice");

    return Stream.of(Arguments.of(read, "The sign in counters could not be read"),
        Arguments.of(add, "A failed sign in could not be counted"),
        Arguments.of(clear, "The failed sign ins could not be cleared"));
  }

  @Test
  void shouldCreateTheStoreOfTheApplication() {
    assertNotNull(BBjThrottleStore.create(WINDOW));
  }

  private BBjThrottleStore createStore() {
    return new BBjThrottleStore(new BBjEntryFile(() -> directory, () -> "shop", Content.COUNTERS),
        WINDOW, clock);
  }
}
