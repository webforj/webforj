package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyFloat;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.webforj.dispatcher.EventListener;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DebouncerTest {

  private TestableDebouncer createTestable(float delay) {
    Debouncer debouncer = spy(new Debouncer(delay));
    AtomicReference<EventListener<Interval.ElapsedEvent>> capturedListener =
        new AtomicReference<>();

    doAnswer(invocation -> {
      capturedListener.set(invocation.getArgument(1));
      return mock(Interval.class);
    }).when(debouncer).createInterval(anyFloat(), any());

    return new TestableDebouncer(debouncer, capturedListener);
  }

  private static class TestableDebouncer {
    final Debouncer debouncer;
    final AtomicReference<EventListener<Interval.ElapsedEvent>> listener;

    TestableDebouncer(Debouncer debouncer,
        AtomicReference<EventListener<Interval.ElapsedEvent>> listener) {
      this.debouncer = debouncer;
      this.listener = listener;
    }

    void triggerTimer() {
      EventListener<Interval.ElapsedEvent> l = listener.get();
      if (l != null) {
        l.onEvent(mock(Interval.ElapsedEvent.class));
      }
    }
  }

  @Test
  @DisplayName("should create debouncer with valid delay")
  void shouldCreate() {
    Debouncer debouncer = new Debouncer(0.5f);
    assertEquals(0.5f, debouncer.getDelay());
  }

  @Test
  @DisplayName("should throw on negative delay")
  void shouldThrowOnNegativeDelay() {
    assertThrows(IllegalArgumentException.class, () -> new Debouncer(-1f));
  }

  @Test
  @DisplayName("should throw on null action")
  void shouldThrowOnNullAction() {
    Debouncer debouncer = new Debouncer(0.5f);
    assertThrows(NullPointerException.class, () -> debouncer.run(null));
  }

  @Test
  @DisplayName("should not invoke immediately on run")
  void shouldNotInvokeImmediately() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.run(counter::incrementAndGet);

    assertEquals(0, counter.get());
    assertTrue(testable.debouncer.isPending());
  }

  @Test
  @DisplayName("should invoke on timer elapsed")
  void shouldInvokeOnTimerElapsed() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.run(counter::incrementAndGet);
    testable.triggerTimer();

    assertEquals(1, counter.get());
    assertFalse(testable.debouncer.isPending());
  }

  @Test
  @DisplayName("should use last action")
  void shouldUseLastAction() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.run(() -> counter.addAndGet(1));
    testable.debouncer.run(() -> counter.addAndGet(10));
    testable.debouncer.run(() -> counter.addAndGet(100));
    testable.triggerTimer();

    assertEquals(100, counter.get());
  }

  @Test
  @DisplayName("should cancel pending action")
  void shouldCancel() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.run(counter::incrementAndGet);
    testable.debouncer.cancel();

    assertFalse(testable.debouncer.isPending());
    testable.triggerTimer();
    assertEquals(0, counter.get());
  }

  @Test
  @DisplayName("should flush pending action")
  void shouldFlush() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.run(counter::incrementAndGet);
    testable.debouncer.flush();

    assertEquals(1, counter.get());
    assertFalse(testable.debouncer.isPending());
  }

  @Test
  @DisplayName("flush should do nothing when not pending")
  void flushShouldDoNothingWhenNotPending() {
    AtomicInteger counter = new AtomicInteger(0);
    TestableDebouncer testable = createTestable(0.5f);

    testable.debouncer.flush();
    assertEquals(0, counter.get());
  }
}
