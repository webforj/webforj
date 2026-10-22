package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.PendingResult;
import com.webforj.bbj.security.fixture.CapturedLog;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAdminTaskRunnerTest {
  private static final Duration LONG = Duration.ofSeconds(10);
  private static final Duration SHORT = Duration.ofMillis(200);
  private final List<CountDownLatch> gates = new ArrayList<>();
  private BBjAdminTaskRunner runner;

  @AfterEach
  void tearDown() {
    gates.forEach(CountDownLatch::countDown);
    if (runner != null) {
      runner.destroy();
    }
  }

  private static Supplier<Function<Runnable, PendingResult<Void>>> createDirectDispatcher() {
    return () -> task -> {
      task.run();

      return PendingResult.completedWith(null);
    };
  }

  private static <R> R getResult(PendingResult<R> result) throws Exception {
    CompletableFuture<R> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  private static long getWorkerThreadCount() {
    return Thread.getAllStackTraces().keySet().stream()
        .filter(thread -> thread.getName().startsWith("bbj-admin-")).count();
  }

  private CountDownLatch createGate() {
    CountDownLatch gate = new CountDownLatch(1);
    gates.add(gate);

    return gate;
  }

  private static void stopUntilReleased(CountDownLatch gate) {
    boolean interrupted = false;
    while (gate.getCount() > 0) {
      try {
        gate.await();
      } catch (InterruptedException e) {
        interrupted = true;
      }
    }

    if (interrupted) {
      Thread.currentThread().interrupt();
    }
  }

  private static Stream<Supplier<PendingResult<Void>>> createRejectedPosts() {
    return Stream.of(() -> PendingResult.completedExceptionallyWith(new IllegalStateException()),
        () -> {
          PendingResult<Void> cancelled = new PendingResult<>();
          cancelled.cancel();

          return cancelled;
        });
  }

  @Test
  void shouldDeliverTheValueFromADaemonWorkerThread() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    AtomicReference<Thread> worker = new AtomicReference<>();

    PendingResult<String> result = runner.start(() -> {
      worker.set(Thread.currentThread());

      return "value";
    }, value -> value + " delivered", failure -> "failed", value -> {
    });

    assertEquals("value delivered", getResult(result));
    assertTrue(worker.get().isDaemon());
    assertTrue(worker.get().getName().startsWith("bbj-admin-"));
  }

  @Test
  void shouldStartNoThreadBeforeTheFirstTask() {
    long before = getWorkerThreadCount();

    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());

    assertEquals(before, getWorkerThreadCount());
  }

  @ParameterizedTest
  @EnumSource(BBjAdminFailure.Kind.class)
  void shouldHandTheKindOfAFailureToTheFailHook(BBjAdminFailure.Kind kind) throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());

    PendingResult<BBjAdminFailure.Kind> result = runner.<String, BBjAdminFailure.Kind>start(() -> {
      throw new BBjAdminFailure(kind, "refused");
    }, value -> null, BBjAdminFailure::getKind, value -> {
    });

    assertEquals(kind, getResult(result));
  }

  @Test
  void shouldTurnAnyOtherExceptionIntoAnUnavailableFailureWithoutCause() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    AtomicReference<BBjAdminFailure> failure = new AtomicReference<>();

    PendingResult<String> result = runner.start(() -> {
      throw new IllegalArgumentException("secret detail");
    }, value -> "delivered", error -> {
      failure.set(error);

      return "failed";
    }, value -> {
    });

    assertEquals("failed", getResult(result));
    assertEquals(BBjAdminFailure.Kind.UNAVAILABLE, failure.get().getKind());
    assertNull(failure.get().getCause());
    assertFalse(failure.get().getMessage().contains("secret"));
  }

  @Test
  void shouldFailAtTheTimeLimitAndDiscardTheLateValue() throws Exception {
    runner = new BBjAdminTaskRunner(SHORT, createDirectDispatcher());
    CountDownLatch lateGate = createGate();
    CountDownLatch blocker = createGate();
    CountDownLatch interrupted = new CountDownLatch(1);
    AtomicBoolean delivered = new AtomicBoolean();
    CompletableFuture<String> discarded = new CompletableFuture<>();
    AtomicReference<Thread> discardThread = new AtomicReference<>();

    PendingResult<String> result = runner.start(() -> {
      try {
        blocker.await();
      } catch (InterruptedException e) {
        interrupted.countDown();
      }

      stopUntilReleased(lateGate);

      return "late";
    }, value -> {
      delivered.set(true);

      return "delivered";
    }, failure -> "failed", value -> {
      discardThread.set(Thread.currentThread());
      discarded.complete(value);
    });

    assertEquals("failed", getResult(result));
    assertTrue(interrupted.await(5, TimeUnit.SECONDS));
    lateGate.countDown();
    assertEquals("late", discarded.get(5, TimeUnit.SECONDS));
    assertFalse(delivered.get());
    assertTrue(discardThread.get().getName().startsWith("bbj-admin-"));
  }

  @Test
  void shouldFailAQueuedTaskAtTheTimeLimitWithoutRunningIt() throws Exception {
    runner = new BBjAdminTaskRunner(SHORT, createDirectDispatcher());
    CountDownLatch gate = createGate();
    for (int count = 0; count < 2; count++) {
      runner.<String, String>start(() -> {
        stopUntilReleased(gate);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      });
    }

    AtomicBoolean ran = new AtomicBoolean();
    PendingResult<String> queued = runner.start(() -> {
      ran.set(true);

      return "value";
    }, value -> value, failure -> "failed", value -> {
    });

    assertEquals("failed", getResult(queued));
    gate.countDown();
    assertFalse(ran.get());
  }

  @Test
  void shouldFailAtOnceWhenTheQueueIsFull() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    CountDownLatch gate = createGate();
    for (int count = 0; count < 6; count++) {
      runner.<String, String>start(() -> {
        stopUntilReleased(gate);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      });
    }

    PendingResult<String> result =
        runner.start(() -> "never", value -> value, failure -> "failed", value -> {
        });

    assertEquals("failed", getResult(result));
  }

  @Test
  void shouldFailAtOnceWhenEveryWorkerIsAbandoned() throws Exception {
    runner = new BBjAdminTaskRunner(SHORT, createDirectDispatcher());
    CountDownLatch gate = createGate();
    List<PendingResult<String>> results = new ArrayList<>();
    for (int count = 0; count < 2; count++) {
      results.add(runner.start(() -> {
        stopUntilReleased(gate);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      }));
    }

    for (PendingResult<String> result : results) {
      assertEquals("failed", getResult(result));
    }

    AtomicBoolean ran = new AtomicBoolean();
    PendingResult<String> result = runner.start(() -> {
      ran.set(true);

      return "never";
    }, value -> value, failure -> "failed", value -> {
    });

    assertTrue(result.isDone());
    assertEquals("failed", getResult(result));
    assertFalse(ran.get());
  }

  @Test
  void shouldSkipTheDeliveryOfACancelledResultAndDiscardTheValue() throws Exception {
    AtomicReference<Runnable> posted = new AtomicReference<>();
    CountDownLatch postedLatch = new CountDownLatch(1);
    runner = new BBjAdminTaskRunner(LONG, () -> task -> {
      posted.set(task);
      postedLatch.countDown();

      return PendingResult.completedWith(null);
    });
    AtomicBoolean delivered = new AtomicBoolean();
    CompletableFuture<Thread> discardThread = new CompletableFuture<>();

    PendingResult<String> result = runner.start(() -> "value", value -> {
      delivered.set(true);

      return value;
    }, failure -> "failed", value -> discardThread.complete(Thread.currentThread()));
    assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
    result.cancel();
    posted.get().run();

    assertFalse(delivered.get());
    assertNotEquals(Thread.currentThread(), discardThread.get(5, TimeUnit.SECONDS));
  }

  @ParameterizedTest
  @MethodSource("createRejectedPosts")
  void shouldDiscardTheValueWhenTheDeliveryCouldNotBePosted(Supplier<PendingResult<Void>> rejection)
      throws Exception {
    runner = new BBjAdminTaskRunner(LONG, () -> task -> rejection.get());
    CompletableFuture<String> discarded = new CompletableFuture<>();

    runner.start(() -> "value", value -> value, failure -> "failed", discarded::complete);

    assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
  }

  @Test
  void shouldDiscardTheValueWhenThePostThrows() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, () -> task -> {
      throw new IllegalStateException();
    });
    CompletableFuture<String> discarded = new CompletableFuture<>();

    runner.start(() -> "value", value -> value, failure -> "failed", discarded::complete);

    assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
  }

  @Test
  void shouldFailAtOnceAfterDestroyAndDestroyTwice() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    runner.destroy();
    runner.destroy();

    PendingResult<String> result =
        runner.start(() -> "never", value -> value, failure -> "failed", value -> {
        });

    assertTrue(result.isDone());
    assertEquals("failed", getResult(result));
  }

  @Test
  void shouldNotRunQueuedTasksAfterDestroy() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    CountDownLatch gate = createGate();
    for (int count = 0; count < 2; count++) {
      runner.<String, String>start(() -> {
        stopUntilReleased(gate);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      });
    }

    AtomicBoolean ran = new AtomicBoolean();
    runner.start(() -> {
      ran.set(true);

      return "value";
    }, value -> value, failure -> "failed", value -> {
    });
    runner.destroy();
    gate.countDown();

    Thread.sleep(300);

    assertFalse(ran.get());
  }

  @Test
  void shouldCompleteExceptionallyAndDiscardWhenTheDeliverHookThrows() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    CompletableFuture<String> discarded = new CompletableFuture<>();

    try (CapturedLog log = new CapturedLog(BBjAdminTaskRunner.class.getPackageName())) {
      PendingResult<String> result = runner.start(() -> "value", value -> {
        throw new IllegalStateException("password");
      }, failure -> "failed", discarded::complete);
      Exception error = assertThrows(Exception.class, () -> getResult(result));

      assertInstanceOf(IllegalStateException.class, error.getCause());
      assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
      assertEquals(List.of("An Admin API task hook failed with java.lang.IllegalStateException"),
          log.getMessages(Level.WARNING));
    }
  }

  @Test
  void shouldCompleteExceptionallyWhenTheFailHookThrowsAtOnce() {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());
    runner.destroy();

    PendingResult<String> result = runner.start(() -> "never", value -> value, failure -> {
      throw new IllegalStateException();
    }, value -> {
    });

    assertTrue(result.isCompletedExceptionally());
  }

  @Test
  void shouldCompleteExceptionallyWhenTheFailHookThrowsAfterTheFailure() throws Exception {
    runner = new BBjAdminTaskRunner(LONG, createDirectDispatcher());

    PendingResult<String> result = runner.<String, String>start(() -> {
      throw new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE, "down");
    }, value -> value, failure -> {
      throw new IllegalStateException();
    }, value -> {
    });

    assertThrows(Exception.class, () -> getResult(result));
  }

  @Test
  void shouldSkipTheFailHookOfACancelledResult() throws Exception {
    AtomicReference<Runnable> posted = new AtomicReference<>();
    CountDownLatch postedLatch = new CountDownLatch(1);
    runner = new BBjAdminTaskRunner(LONG, () -> task -> {
      posted.set(task);
      postedLatch.countDown();

      return PendingResult.completedWith(null);
    });
    AtomicBoolean failed = new AtomicBoolean();

    PendingResult<String> result = runner.<String, String>start(() -> {
      throw new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE, "down");
    }, value -> value, failure -> {
      failed.set(true);

      return "failed";
    }, value -> {
    });
    assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
    result.cancel();
    posted.get().run();

    assertFalse(failed.get());
    assertTrue(result.isCancelled());
  }

  @Test
  void shouldRejectNullArguments() {
    assertThrows(NullPointerException.class,
        () -> new BBjAdminTaskRunner(null, createDirectDispatcher()));
    assertThrows(NullPointerException.class, () -> new BBjAdminTaskRunner(LONG, null));
  }
}
