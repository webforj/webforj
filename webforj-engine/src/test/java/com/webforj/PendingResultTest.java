package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PendingResultTest {

  @Test
  @DisplayName("thenApply should transform result with provided function")
  void testThenApply() {
    PendingResult<String> pending = PendingResult.completedWith("test");
    AtomicReference<String> result = new AtomicReference<>();

    pending.thenApply(String::toUpperCase).thenAccept(result::set);

    assertTrue(pending.isDone());
    assertEquals("TEST", result.get());
  }

  @Test
  @DisplayName("thenAccept should consume result without altering future's result type")
  void testThenAccept() {
    PendingResult<String> pending = PendingResult.completedWith("test");
    StringBuilder result = new StringBuilder();

    pending.thenAccept(result::append);
    assertEquals("test", result.toString());
  }

  @Test
  @DisplayName("exceptionally should handle exceptions and provide fallback")
  void testExceptionally() {
    PendingResult<String> pending =
        PendingResult.completedExceptionallyWith(new RuntimeException("Test Exception"));
    AtomicReference<String> result = new AtomicReference<>();

    pending.exceptionally(ex -> {
      result.set("Exception Handled");
      return null;
    });

    assertTrue(pending.isCompletedExceptionally());
    assertEquals("Exception Handled", result.get());
  }

  @Test
  @DisplayName("thenCompose should chain another AsyncOperation")
  void testThenCompose() {
    PendingResult<String> pending = PendingResult.completedWith("composed");
    AtomicReference<String> result = new AtomicReference<>();

    pending.thenCompose(s -> new PendingResult<>(PendingResult.completedWith(s + " result")))
        .thenAccept(result::set);

    assertEquals("composed result", result.get());
  }

  @Test
  @DisplayName("thenCombine should combine results of two AsyncOperations")
  void testThenCombine() {
    PendingResult<String> firstOperation = PendingResult.completedWith("combined");
    PendingResult<String> secondOperation = PendingResult.completedWith(" result");
    AtomicReference<String> result = new AtomicReference<>();

    firstOperation.thenCombine(secondOperation, (s1, s2) -> s1 + s2).thenAccept(result::set);

    assertEquals("combined result", result.get());
  }

  @Test
  @DisplayName("complete should successfully complete the pending with a provided result")
  void testComplete() {
    PendingResult<String> pending = new PendingResult<>();
    AtomicReference<String> result = new AtomicReference<>();

    assertFalse(pending.isDone());
    pending.complete("completed");
    pending.thenAccept(result::set);

    assertTrue(pending.isDone());
    assertEquals("completed", result.get());
  }

  @Test
  @DisplayName("completeExceptionally should complete the pending with an exception")
  void testCompleteExceptionally() {
    PendingResult<String> pending = new PendingResult<>();
    AtomicReference<Throwable> exception = new AtomicReference<>();

    assertFalse(pending.isDone());
    pending.completeExceptionally(new RuntimeException("Failure"));

    pending.exceptionally(ex -> {
      exception.set(ex);
      return null;
    });

    assertTrue(pending.isDone());
    assertTrue(exception.get() instanceof RuntimeException);
    assertEquals("Failure", exception.get().getMessage());
  }

  @Test
  @DisplayName("allOf should wait for all PendingResults to complete")
  void testAllOf() {
    PendingResult<String> first = PendingResult.completedWith("first");
    PendingResult<String> second = PendingResult.completedWith("second");
    PendingResult<String> third =
        PendingResult.completedExceptionallyWith(new RuntimeException("Error"));

    PendingResult<Void> combined = PendingResult.allOf(first, second, third);

    assertTrue(combined.isDone());
    assertTrue(combined.isCompletedExceptionally());
  }
}
