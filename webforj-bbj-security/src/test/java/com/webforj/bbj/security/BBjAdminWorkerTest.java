package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BBjAdminWorkerTest {
  private static final Duration LONG = Duration.ofSeconds(10);
  private static final Duration SHORT = Duration.ofMillis(200);
  private final List<CountDownLatch> gates = new ArrayList<>();
  private final List<LogRecord> records = new CopyOnWriteArrayList<>();
  private final CountDownLatch warned = new CountDownLatch(1);
  private final Logger logger = Logger.getLogger(BBjAdminWorker.class.getName());
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord logRecord) {
      records.add(logRecord);
      if (logRecord.getLevel() == Level.WARNING) {
        warned.countDown();
      }
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
  private MockedStatic<Environment> environmentMock;
  private Level previousLevel;
  private BBjAdminWorker worker;

  @BeforeEach
  void setUp() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
    environmentMock = mockStatic(Environment.class);
    var mockEvniornment = mock(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(mockEvniornment);
  }

  @AfterEach
  void tearDown() {
    gates.forEach(CountDownLatch::countDown);
    if (worker != null) {
      worker.destroy();
    }

    awaitWorkerThreads();
    environmentMock.close();
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  private static void awaitWorkerThreads() {
    for (Thread thread : Thread.getAllStackTraces().keySet()) {
      if (thread.getName().startsWith("bbj-admin")) {
        try {
          thread.join(5000);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();

          return;
        }
      }
    }
  }

  private static BBjAdminWorker createWorker(Duration timeout,
      Function<Runnable, PendingResult<Void>> post) {
    return new BBjAdminWorker(timeout, (environment, task) -> post.apply(task));
  }

  private static PendingResult<Void> sendDirectly(Runnable task) {
    task.run();

    return PendingResult.completedWith(null);
  }

  private static <R> R getResult(PendingResult<R> result) throws Exception {
    CompletableFuture<R> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  private List<String> getWarnings() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.WARNING)
        .map(formatter::formatMessage).toList();
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

  private static void stopUntilInterrupted(CountDownLatch interrupted) {
    try {
      new CountDownLatch(1).await();
    } catch (InterruptedException e) {
      interrupted.countDown();
    }
  }

  static Stream<Arguments> createRejectedPosts() {
    Function<Runnable, PendingResult<Void>> throwing = task -> {
      throw new IllegalStateException();
    };
    Function<Runnable, PendingResult<Void>> failed =
        task -> PendingResult.completedExceptionallyWith(new IllegalStateException());
    Function<Runnable, PendingResult<Void>> cancelled = task -> {
      PendingResult<Void> result = new PendingResult<>();
      result.cancel();

      return result;
    };

    return Stream.of(Arguments.of(Named.of("runLater throws", throwing)),
        Arguments.of(Named.of("runLater returns a failed result", failed)),
        Arguments.of(Named.of("runLater returns a cancelled result", cancelled)));
  }

  @Nested
  class Refusals {

    @Test
    void shouldRefuseStartAfterDestroy() throws Exception {
      worker = new BBjAdminWorker(LONG);
      worker.destroy();
      AtomicBoolean ran = new AtomicBoolean();

      PendingResult<Kind> result = worker.start(() -> {
        ran.set(true);

        return "never";
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertTrue(result.isDone());
      assertEquals(Kind.UNAVAILABLE, getResult(result));
      assertFalse(ran.get());
    }

    @Test
    void shouldRefuseStartWhileBothWorkersAreAbandoned() throws Exception {
      worker = new BBjAdminWorker(SHORT);
      CountDownLatch interrupted = new CountDownLatch(2);
      CountDownLatch release = createGate();
      for (int count = 0; count < 2; count++) {
        worker.<String, String>start(() -> {
          stopUntilInterrupted(interrupted);
          stopUntilReleased(release);

          return "late";
        }, value -> value, failure -> "failed", value -> {
        });
      }

      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
      AtomicBoolean ran = new AtomicBoolean();
      PendingResult<Kind> result = worker.start(() -> {
        ran.set(true);

        return "never";
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertTrue(result.isDone());
      assertEquals(Kind.UNAVAILABLE, getResult(result));
      assertFalse(ran.get());
    }

    @Test
    void shouldRefuseStartWhenTwoAreRunningAndFourAreWaiting() throws Exception {
      worker = new BBjAdminWorker(LONG);
      CountDownLatch release = createGate();
      for (int count = 0; count < 6; count++) {
        worker.<String, String>start(() -> {
          stopUntilReleased(release);

          return "value";
        }, value -> value, failure -> "failed", value -> {
        });
      }

      AtomicBoolean ran = new AtomicBoolean();
      PendingResult<Kind> result = worker.start(() -> {
        ran.set(true);

        return "never";
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertTrue(result.isDone());
      assertEquals(Kind.UNAVAILABLE, getResult(result));
      assertFalse(ran.get());
    }

    @Test
    void shouldCompleteExceptionallyAndWarnWhenTheFailHookThrowsOnRefusal() {
      worker = new BBjAdminWorker(LONG);
      worker.destroy();

      PendingResult<String> result = worker.start(() -> "never", value -> value, failure -> {
        throw new IllegalStateException("password");
      }, value -> {
      });

      assertTrue(result.isCompletedExceptionally());
      assertEquals(1, getWarnings().size());
      assertTrue(getWarnings().get(0).contains("java.lang.IllegalStateException"));
      assertFalse(getWarnings().get(0).contains("password"));
    }
  }

  @Nested
  class NullArguments {

    @Test
    void shouldRejectNullTimeout() {
      Duration timeout = null;

      assertThrows(NullPointerException.class, () -> new BBjAdminWorker(timeout));
    }

    @Test
    void shouldRejectNullDispatcher() {
      BiFunction<Environment, Runnable, PendingResult<Void>> dispatcher = null;

      assertThrows(NullPointerException.class, () -> new BBjAdminWorker(LONG, dispatcher));
    }

    @ParameterizedTest
    @MethodSource("createNullCalls")
    void shouldRejectNullArgumentOfStart(Consumer<BBjAdminWorker> call) {
      worker = new BBjAdminWorker(LONG);

      assertThrows(NullPointerException.class, () -> call.accept(worker));
    }

    private static Stream<Arguments> createNullCalls() {
      Consumer<BBjAdminWorker> nullWork = target -> target.<String, String>start(null,
          value -> value, failure -> "failed", value -> {
          });
      Consumer<BBjAdminWorker> nullDeliver = target -> target.<String, String>start(() -> "value",
          null, failure -> "failed", value -> {
          });
      Consumer<BBjAdminWorker> nullFail =
          target -> target.<String, String>start(() -> "value", value -> value, null, value -> {
          });
      Consumer<BBjAdminWorker> nullDiscard = target -> target.<String, String>start(() -> "value",
          value -> value, failure -> "failed", null);

      return Stream.of(Arguments.of(Named.of("work", nullWork)),
          Arguments.of(Named.of("deliver", nullDeliver)), Arguments.of(Named.of("fail", nullFail)),
          Arguments.of(Named.of("discard", nullDiscard)));
    }
  }

  @Nested
  class Outcomes {

    @Test
    void shouldDeliverTheValueAndCompleteTheResult() throws Exception {
      worker = createWorker(LONG, BBjAdminWorkerTest::sendDirectly);

      PendingResult<String> result =
          worker.start(() -> "value", value -> value + " delivered", failure -> "failed", value -> {
          });

      assertEquals("value delivered", getResult(result));
    }

    @Test
    void shouldHandTheEnvironmentOfTheCallingThreadToTheDispatcher() throws Exception {
      Environment current = mock(Environment.class);
      environmentMock.when(Environment::getCurrent).thenReturn(current);
      AtomicReference<Environment> dispatched = new AtomicReference<>();
      worker = new BBjAdminWorker(LONG, (environment, task) -> {
        dispatched.set(environment);

        return sendDirectly(task);
      });

      PendingResult<String> result =
          worker.start(() -> "value", value -> value, failure -> "failed", value -> {
          });

      assertEquals("value", getResult(result));
      assertSame(current, dispatched.get());
    }

    @ParameterizedTest
    @EnumSource(Kind.class)
    void shouldHandTheKindOfThrownFailureToTheFailHook(Kind kind) throws Exception {
      worker = createWorker(LONG, BBjAdminWorkerTest::sendDirectly);

      PendingResult<Kind> result = worker.<String, Kind>start(() -> {
        throw new BBjAdminFailure(kind, "refused");
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertEquals(kind, getResult(result));
    }

    @Test
    void shouldTurnAnyOtherExceptionIntoAnUnavailableFailureWithoutItsDetail() throws Exception {
      worker = createWorker(LONG, BBjAdminWorkerTest::sendDirectly);
      AtomicReference<BBjAdminFailure> failure = new AtomicReference<>();

      PendingResult<String> result = worker.start(() -> {
        throw new IllegalArgumentException("secret detail");
      }, value -> "delivered", error -> {
        failure.set(error);

        return "failed";
      }, value -> {
      });

      assertEquals("failed", getResult(result));
      assertEquals(Kind.UNAVAILABLE, failure.get().getKind());
      assertNull(failure.get().getCause());
      assertFalse(failure.get().getMessage().contains("secret"));
      assertTrue(records.stream()
          .noneMatch(it -> new SimpleFormatter().formatMessage(it).contains("secret")));
    }
  }

  @Nested
  class Timeouts {

    @Test
    void shouldFailInterruptTheWorkerAndWarnOnceAtTheTimeLimit() throws Exception {
      worker = createWorker(SHORT, BBjAdminWorkerTest::sendDirectly);
      CountDownLatch interrupted = new CountDownLatch(1);
      CountDownLatch release = createGate();

      PendingResult<Kind> result = worker.<String, Kind>start(() -> {
        stopUntilInterrupted(interrupted);
        stopUntilReleased(release);

        return "late";
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertEquals(Kind.UNAVAILABLE, getResult(result));
      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
      assertEquals(1, getWarnings().size());
    }

    @Test
    void shouldDiscardLateValueAndNeverDeliverIt() throws Exception {
      worker = createWorker(SHORT, BBjAdminWorkerTest::sendDirectly);
      CountDownLatch interrupted = new CountDownLatch(1);
      CountDownLatch release = createGate();
      AtomicBoolean delivered = new AtomicBoolean();
      CompletableFuture<String> discarded = new CompletableFuture<>();

      PendingResult<String> result = worker.start(() -> {
        stopUntilInterrupted(interrupted);
        stopUntilReleased(release);

        return "late";
      }, value -> {
        delivered.set(true);

        return "delivered";
      }, failure -> "failed", discarded::complete);

      assertEquals("failed", getResult(result));
      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
      release.countDown();
      assertEquals("late", discarded.get(5, TimeUnit.SECONDS));
      assertFalse(delivered.get());
    }

    @Test
    void shouldFailQueuedTaskAtTheTimeLimitWithoutRunningIt() throws Exception {
      worker = createWorker(SHORT, BBjAdminWorkerTest::sendDirectly);
      CountDownLatch release = createGate();
      CountDownLatch discarded = new CountDownLatch(2);
      for (int count = 0; count < 2; count++) {
        worker.<String, String>start(() -> {
          stopUntilReleased(release);

          return "value";
        }, value -> value, failure -> "failed", value -> discarded.countDown());
      }

      AtomicBoolean ran = new AtomicBoolean();
      PendingResult<Kind> queued = worker.start(() -> {
        ran.set(true);

        return "value";
      }, value -> null, BBjAdminFailure::getKind, value -> {
      });

      assertEquals(Kind.UNAVAILABLE, getResult(queued));
      release.countDown();
      assertTrue(discarded.await(5, TimeUnit.SECONDS));
      assertFalse(ran.get());
    }
  }

  @Nested
  class AbandonedWorkers {

    @Test
    void shouldAcceptStartsAgainWhenAnAbandonedWorkerReturns() throws Exception {
      worker = new BBjAdminWorker(SHORT);
      CountDownLatch interrupted = new CountDownLatch(2);
      List<CountDownLatch> releases = new ArrayList<>();
      for (int count = 0; count < 2; count++) {
        CountDownLatch release = createGate();
        releases.add(release);
        worker.<String, String>start(() -> {
          stopUntilInterrupted(interrupted);
          stopUntilReleased(release);

          return "late";
        }, value -> value, failure -> "failed", value -> {
        });
      }

      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
      PendingResult<Kind> refused =
          worker.start(() -> "never", value -> null, BBjAdminFailure::getKind, value -> {
          });
      assertTrue(refused.isDone());
      assertEquals(Kind.UNAVAILABLE, getResult(refused));

      releases.get(0).countDown();
      CountDownLatch ran = new CountDownLatch(1);
      boolean accepted = false;
      for (int attempt = 0; attempt < 100 && !accepted; attempt++) {
        worker.<String, String>start(() -> {
          ran.countDown();

          return "value";
        }, value -> value, failure -> "failed", value -> {
        });
        accepted = ran.await(50, TimeUnit.MILLISECONDS);
      }

      assertTrue(accepted);
    }
  }

  @Nested
  class CancelledResults {

    @Test
    void shouldDiscardTheValueOfCancelledResult() throws Exception {
      AtomicReference<Runnable> posted = new AtomicReference<>();
      CountDownLatch postedLatch = new CountDownLatch(1);
      worker = createWorker(LONG, task -> {
        posted.set(task);
        postedLatch.countDown();

        return PendingResult.completedWith(null);
      });
      AtomicBoolean delivered = new AtomicBoolean();
      CompletableFuture<String> discarded = new CompletableFuture<>();

      PendingResult<String> result = worker.start(() -> "value", value -> {
        delivered.set(true);

        return value;
      }, failure -> "failed", discarded::complete);
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
      result.cancel();
      posted.get().run();

      assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
      assertFalse(delivered.get());
    }

    @Test
    void shouldDropTheFailureOfCancelledResult() throws Exception {
      AtomicReference<Runnable> posted = new AtomicReference<>();
      CountDownLatch postedLatch = new CountDownLatch(1);
      worker = createWorker(LONG, task -> {
        posted.set(task);
        postedLatch.countDown();

        return PendingResult.completedWith(null);
      });
      AtomicBoolean failed = new AtomicBoolean();
      AtomicBoolean discarded = new AtomicBoolean();

      PendingResult<String> result = worker.<String, String>start(() -> {
        throw new BBjAdminFailure(Kind.UNAVAILABLE, "down");
      }, value -> value, failure -> {
        failed.set(true);

        return "failed";
      }, value -> discarded.set(true));
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
      result.cancel();
      posted.get().run();

      assertFalse(failed.get());
      assertFalse(discarded.get());
      assertTrue(result.isCancelled());
    }
  }

  @Nested
  class ThrowingHooks {

    @Test
    void shouldCompleteExceptionallyAndDiscardWhenTheDeliverHookThrows() throws Exception {
      worker = createWorker(LONG, BBjAdminWorkerTest::sendDirectly);
      CompletableFuture<String> discarded = new CompletableFuture<>();

      PendingResult<String> result = worker.start(() -> "value", value -> {
        throw new IllegalStateException("password");
      }, failure -> "failed", discarded::complete);
      Exception error = assertThrows(Exception.class, () -> getResult(result));

      assertInstanceOf(IllegalStateException.class, error.getCause());
      assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
      assertEquals(1, getWarnings().size());
      assertTrue(getWarnings().get(0).contains("java.lang.IllegalStateException"));
      assertFalse(getWarnings().get(0).contains("password"));
    }

    @Test
    void shouldCompleteExceptionallyWhenTheFailHookThrowsAfterFailure() {
      worker = createWorker(LONG, BBjAdminWorkerTest::sendDirectly);

      PendingResult<String> result = worker.<String, String>start(() -> {
        throw new BBjAdminFailure(Kind.UNAVAILABLE, "down");
      }, value -> value, failure -> {
        throw new IllegalStateException("password");
      }, value -> {
      });
      Exception error = assertThrows(Exception.class, () -> getResult(result));

      assertInstanceOf(IllegalStateException.class, error.getCause());
      assertEquals(1, getWarnings().size());
      assertTrue(getWarnings().get(0).contains("java.lang.IllegalStateException"));
      assertFalse(getWarnings().get(0).contains("password"));
    }

    @Test
    void shouldWarnWithTheClassNameOnlyWhenTheDiscardHookThrows() throws Exception {
      AtomicReference<Runnable> posted = new AtomicReference<>();
      CountDownLatch postedLatch = new CountDownLatch(1);
      worker = createWorker(LONG, task -> {
        posted.set(task);
        postedLatch.countDown();

        return PendingResult.completedWith(null);
      });

      PendingResult<String> result =
          worker.start(() -> "value", value -> value, failure -> "failed", value -> {
            throw new IllegalStateException("password");
          });
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
      result.cancel();
      posted.get().run();

      assertTrue(warned.await(5, TimeUnit.SECONDS));
      assertEquals(1, getWarnings().size());
      assertTrue(getWarnings().get(0).contains("java.lang.IllegalStateException"));
      assertFalse(getWarnings().get(0).contains("password"));
    }
  }

  @Nested
  class DeliveryFailures {

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminWorkerTest#createRejectedPosts")
    void shouldDiscardTheValueExactlyOnceWhenTheDeliveryCannotBePosted(
        Function<Runnable, PendingResult<Void>> post) throws Exception {
      worker = createWorker(LONG, post);
      AtomicInteger discardCount = new AtomicInteger();
      CompletableFuture<String> discarded = new CompletableFuture<>();

      worker.start(() -> "value", value -> value, failure -> "failed", value -> {
        discardCount.incrementAndGet();
        discarded.complete(value);
      });

      assertEquals("value", discarded.get(5, TimeUnit.SECONDS));
      assertEquals(1, discardCount.get());
    }
  }

  @Nested
  class Threads {

    @Test
    void shouldRunWorkOnDaemonThreadsNamedInOrder() throws Exception {
      worker = new BBjAdminWorker(LONG);
      CountDownLatch started = new CountDownLatch(2);
      CountDownLatch release = createGate();
      ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<>();
      for (int count = 0; count < 2; count++) {
        worker.<String, String>start(() -> {
          threads.add(Thread.currentThread());
          started.countDown();
          stopUntilReleased(release);

          return "value";
        }, value -> value, failure -> "failed", value -> {
        });
      }

      assertTrue(started.await(5, TimeUnit.SECONDS));
      assertEquals(Set.of("bbj-admin-1", "bbj-admin-2"),
          Set.copyOf(threads.stream().map(Thread::getName).toList()));
      assertTrue(threads.stream().allMatch(Thread::isDaemon));
    }

    @Test
    void shouldRunTheDeadlineOnDaemonTimerThread() throws Exception {
      worker = createWorker(SHORT, BBjAdminWorkerTest::sendDirectly);
      CountDownLatch release = createGate();

      PendingResult<Thread> result = worker.<String, Thread>start(() -> {
        stopUntilReleased(release);

        return "late";
      }, value -> null, failure -> Thread.currentThread(), value -> {
      });

      Thread timer = getResult(result);
      assertEquals("bbj-admin-timer", timer.getName());
      assertTrue(timer.isDaemon());
    }

    @Test
    void shouldDiscardLateValueOnTheWorkerThread() throws Exception {
      worker = createWorker(SHORT, BBjAdminWorkerTest::sendDirectly);
      CountDownLatch interrupted = new CountDownLatch(1);
      CountDownLatch release = createGate();
      AtomicReference<Thread> workerThread = new AtomicReference<>();
      CompletableFuture<Thread> discardThread = new CompletableFuture<>();

      worker.<String, String>start(() -> {
        workerThread.set(Thread.currentThread());
        stopUntilInterrupted(interrupted);
        stopUntilReleased(release);

        return "late";
      }, value -> value, failure -> "failed",
          value -> discardThread.complete(Thread.currentThread()));
      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
      release.countDown();

      assertEquals(workerThread.get(), discardThread.get(5, TimeUnit.SECONDS));
    }

    @Test
    void shouldNeverDiscardOnTheThreadThatDelivers() throws Exception {
      AtomicReference<Runnable> posted = new AtomicReference<>();
      CountDownLatch postedLatch = new CountDownLatch(1);
      worker = createWorker(LONG, task -> {
        posted.set(task);
        postedLatch.countDown();

        return PendingResult.completedWith(null);
      });
      CompletableFuture<Thread> discardThread = new CompletableFuture<>();

      PendingResult<String> result = worker.start(() -> "value", value -> value,
          failure -> "failed", value -> discardThread.complete(Thread.currentThread()));
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
      result.cancel();
      posted.get().run();

      assertNotEquals(Thread.currentThread(), discardThread.get(5, TimeUnit.SECONDS));
    }
  }

  @Nested
  class Destroy {

    @Test
    void shouldInterruptRunningWork() throws Exception {
      worker = new BBjAdminWorker(LONG);
      CountDownLatch started = new CountDownLatch(1);
      CountDownLatch interrupted = new CountDownLatch(1);
      CountDownLatch release = createGate();

      worker.<String, String>start(() -> {
        started.countDown();
        stopUntilInterrupted(interrupted);
        stopUntilReleased(release);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      });
      assertTrue(started.await(5, TimeUnit.SECONDS));
      worker.destroy();

      assertTrue(interrupted.await(5, TimeUnit.SECONDS));
    }

    @Test
    void shouldNeverRunQueuedWork() throws Exception {
      worker = new BBjAdminWorker(LONG);
      CountDownLatch started = new CountDownLatch(2);
      CountDownLatch finished = new CountDownLatch(2);
      CountDownLatch release = createGate();
      for (int count = 0; count < 2; count++) {
        worker.<String, String>start(() -> {
          started.countDown();
          stopUntilReleased(release);
          finished.countDown();

          return "value";
        }, value -> value, failure -> "failed", value -> {
        });
      }

      assertTrue(started.await(5, TimeUnit.SECONDS));
      AtomicBoolean ran = new AtomicBoolean();
      worker.<String, String>start(() -> {
        ran.set(true);

        return "value";
      }, value -> value, failure -> "failed", value -> {
      });
      worker.destroy();
      release.countDown();

      assertTrue(finished.await(5, TimeUnit.SECONDS));
      assertFalse(ran.get());
    }

    @Test
    void shouldDoNothingWhenCalledTwice() {
      worker = new BBjAdminWorker(LONG);

      assertDoesNotThrow(worker::destroy);
      assertDoesNotThrow(worker::destroy);
    }
  }
}
