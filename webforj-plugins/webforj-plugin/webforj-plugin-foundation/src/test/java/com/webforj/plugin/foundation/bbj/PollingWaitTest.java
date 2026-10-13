package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.PollingWait.Condition;
import java.io.IOException;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PollingWaitTest {

  private long now;
  private List<Duration> pauses;
  private List<String> log;

  @BeforeEach
  void initClockAtZero() {
    now = 0;
    pauses = new ArrayList<>();
    log = new ArrayList<>();
  }

  @Test
  void shouldEndOnceTheConditionHoldsWithTheTimeWaited() throws IOException {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(false, false, true));
    PollingWait wait = createWait(Duration.ofSeconds(10), Duration.ofSeconds(2)).build();

    Optional<Duration> waited = wait.apply(answers::pop);

    assertEquals(Optional.of(Duration.ofSeconds(4)), waited);
    assertEquals(List.of(Duration.ofSeconds(2), Duration.ofSeconds(2)), pauses);
  }

  @Test
  void shouldEndWithoutPauseWhenTheConditionHoldsAtOnce() throws IOException {
    PollingWait wait = createWait(Duration.ofSeconds(10), Duration.ofSeconds(2)).build();

    Optional<Duration> waited = wait.apply(() -> true);

    assertEquals(Optional.of(Duration.ZERO), waited);
    assertEquals(List.of(), pauses);
  }

  @Test
  void shouldEndEmptyOnceTheTimeoutPassed() throws IOException {
    List<Long> checks = new ArrayList<>();
    PollingWait wait = createWait(Duration.ofSeconds(1), Duration.ofMillis(250)).build();

    Optional<Duration> waited = wait.apply(() -> {
      checks.add(now);

      return false;
    });

    assertEquals(Optional.empty(), waited);
    assertEquals(List.of(0L, 250_000_000L, 500_000_000L, 750_000_000L, 1_000_000_000L), checks);
  }

  @Test
  void shouldReportTheWaitAtTheProgressInterval() throws IOException {
    PollingWait wait = createWait(Duration.ofSeconds(60), Duration.ofSeconds(4)).setLog(log::add)
        .setProgressMessage("Waiting for BBjServices, ").build();

    wait.apply(() -> now >= Duration.ofSeconds(24).toNanos());

    assertEquals(
        List.of("Waiting for BBjServices, 12 seconds", "Waiting for BBjServices, 20 seconds"), log);
  }

  @Test
  void shouldReportNothingWithoutProgressMessage() throws IOException {
    PollingWait wait = createWait(Duration.ofSeconds(60), Duration.ofSeconds(4)).setLog(log::add)
        .setProgressInterval(Duration.ofSeconds(1)).build();

    wait.apply(() -> now >= Duration.ofSeconds(8).toNanos());

    assertEquals(List.of(), log);
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenThePauseIsInterrupted() {
    PollingWait wait =
        PollingWait.create("BBjServices restart", Duration.ofSeconds(10), Duration.ofSeconds(1))
            .setClock(() -> now).setSleeper(duration -> {
              throw new InterruptedException();
            }).build();
    Condition<RuntimeException> condition = () -> false;

    IOException failure = assertThrows(IOException.class, () -> wait.apply(condition));

    assertEquals("BBjServices restart was interrupted", failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenTheCheckIsInterrupted() {
    PollingWait wait = createWait(Duration.ofSeconds(10), Duration.ofSeconds(1)).build();
    Condition<RuntimeException> condition = () -> {
      throw new InterruptedException();
    };

    IOException failure = assertThrows(IOException.class, () -> wait.apply(condition));

    assertEquals("BBjServices start was interrupted", failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
  }

  @Test
  void shouldEndWithTheFailureOfTheCheck() {
    IOException refused = new IOException("BBjServices did not start");
    PollingWait wait = createWait(Duration.ofSeconds(10), Duration.ofSeconds(1)).build();
    Condition<IOException> condition = () -> {
      throw refused;
    };

    IOException failure = assertThrows(IOException.class, () -> wait.apply(condition));

    assertSame(refused, failure);
    assertEquals(List.of(), pauses);
  }

  private PollingWait.Builder createWait(Duration timeout, Duration interval) {
    return PollingWait.create("BBjServices start", timeout, interval).setClock(() -> now)
        .setSleeper(duration -> {
          pauses.add(duration);
          now += duration.toNanos();
        });
  }
}
