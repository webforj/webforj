package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.webforj.Environment;
import com.webforj.PendingResult;

/**
 * Runs blocking BBj Admin API calls away from the calling thread and hands each outcome back to the
 * environment of the caller.
 *
 * <p>
 * At most two calls run at once and up to four more wait for a free thread. A call that does not
 * finish within the time limit is reported as unavailable and abandoned. While two abandoned calls
 * are still running, and after the worker is destroyed, new calls are refused and reported as
 * unavailable.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjAdminWorker {
  private static final Logger logger = System.getLogger(BBjAdminWorker.class.getName());
  private static final int WORKER_COUNT = 2;
  private static final int QUEUE_SIZE = 4;
  private static final long KEEP_ALIVE_SECONDS = 30;
  private final Duration timeout;
  private final BiFunction<Environment, Runnable, PendingResult<Void>> dispatcher;
  private final AtomicInteger abandonedCount = new AtomicInteger();
  private final AtomicInteger threadCount = new AtomicInteger();
  private final ThreadPoolExecutor workers;
  private final ScheduledThreadPoolExecutor timer;
  private boolean destroyed;

  /**
   * Creates a worker that hands outcomes back through {@link Environment#runLater}.
   *
   * @param timeout the time limit of one call
   *
   * @throws NullPointerException if {@code timeout} is {@code null}
   */
  BBjAdminWorker(Duration timeout) {
    this(timeout, Environment::runLater);
  }

  /**
   * Creates a worker that hands outcomes back through the given dispatcher.
   *
   * @param timeout the time limit of one call
   * @param dispatcher runs a task in the given environment and completes when the task has run
   *
   * @throws NullPointerException if any parameter is {@code null}
   */
  BBjAdminWorker(Duration timeout,
      BiFunction<Environment, Runnable, PendingResult<Void>> dispatcher) {
    this.timeout = Objects.requireNonNull(timeout, "timeout");
    this.dispatcher = Objects.requireNonNull(dispatcher, "dispatcher");
    workers = new ThreadPoolExecutor(WORKER_COUNT, WORKER_COUNT, KEEP_ALIVE_SECONDS,
        TimeUnit.SECONDS, new ArrayBlockingQueue<>(QUEUE_SIZE),
        runnable -> createThread(runnable, "bbj-admin-" + threadCount.incrementAndGet()));
    workers.allowCoreThreadTimeOut(true);
    timer =
        new ScheduledThreadPoolExecutor(1, runnable -> createThread(runnable, "bbj-admin-timer"));
    timer.setKeepAliveTime(KEEP_ALIVE_SECONDS, TimeUnit.SECONDS);
    timer.allowCoreThreadTimeOut(true);
    timer.setRemoveOnCancelPolicy(true);
  }

  /**
   * Starts a call on a worker thread.
   *
   * <p>
   * {@code work} runs on a worker thread. When it returns a value, {@code deliver} turns the value
   * into the result. When it throws a {@link BBjAdminFailure}, {@code fail} turns that failure into
   * the result. Any other runtime exception of {@code work} is reported to {@code fail} as an
   * unavailable failure. Both hooks run in the environment that was current when the call started.
   * </p>
   *
   * <p>
   * When the call misses the time limit, or is refused, {@code fail} receives an unavailable
   * failure. A value that arrives too late, or whose result was cancelled, is passed to
   * {@code discard} instead. {@code discard} runs on a worker thread or on a separate thread. A
   * hook that throws completes the result exceptionally.
   * </p>
   *
   * @param <T> the type of the value of the call
   * @param <R> the type of the result
   * @param work the blocking call
   * @param deliver turns the value of the call into the result
   * @param fail turns a failure into the result
   * @param discard releases a value that is not delivered
   *
   * @return the pending result
   * @throws NullPointerException if any parameter is {@code null}
   */
  <T, R> PendingResult<R> start(Supplier<? extends T> work,
      Function<? super T, ? extends R> deliver, Function<? super BBjAdminFailure, ? extends R> fail,
      Consumer<? super T> discard) {
    Objects.requireNonNull(work, "work");
    Objects.requireNonNull(deliver, "deliver");
    Objects.requireNonNull(fail, "fail");
    Objects.requireNonNull(discard, "discard");
    Task<T, R> task = new Task<>(work, deliver, fail, discard);
    if (!isAcceptingTasks()) {
      return task.createFailedResult();
    }

    try {
      task.start(workers, timer);
    } catch (RejectedExecutionException e) {
      return task.createFailedResult();
    }

    return task.result;
  }

  /**
   * Stops the worker threads and refuses every later call.
   *
   * <p>
   * Calling it again has no effect.
   * </p>
   */
  void destroy() {
    synchronized (this) {
      if (destroyed) {
        return;
      }

      destroyed = true;
    }

    workers.shutdownNow();
    timer.shutdownNow();
  }

  private synchronized boolean isAcceptingTasks() {
    return !destroyed && abandonedCount.get() < WORKER_COUNT;
  }

  private static Thread createThread(Runnable action, String name) {
    Thread thread = new Thread(action, name);
    thread.setDaemon(true);

    return thread;
  }

  private static void startDiscardThread(Runnable action) {
    createThread(action, "bbj-admin-discard").start();
  }

  private static BBjAdminFailure createUnavailableFailure() {
    return new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE,
        "The Admin API call could not be completed");
  }

  /**
   * One call with its hooks, its time limit and its pending result.
   *
   * @param <T> the type of the value of the call
   * @param <R> the type of the result
   */
  private final class Task<T, R> implements Runnable {
    private final Supplier<? extends T> work;
    private final Function<? super T, ? extends R> deliver;
    private final Function<? super BBjAdminFailure, ? extends R> fail;
    private final Consumer<? super T> discard;
    private final Environment environment;
    private final PendingResult<R> result = new PendingResult<>();
    private final AtomicBoolean settled = new AtomicBoolean();
    private final AtomicBoolean delivered = new AtomicBoolean();
    private volatile Thread worker;
    private boolean finished;
    private boolean abandoned;
    private ScheduledFuture<?> deadline;

    Task(Supplier<? extends T> work, Function<? super T, ? extends R> deliver,
        Function<? super BBjAdminFailure, ? extends R> fail, Consumer<? super T> discard) {
      this.work = work;
      this.deliver = deliver;
      this.fail = fail;
      this.discard = discard;
      this.environment = Environment.getCurrent();
    }

    @Override
    public void run() {
      if (settled.get()) {
        return;
      }

      synchronized (this) {
        worker = Thread.currentThread();
      }

      try {
        BBjAdminFailure failure = null;
        T value = null;
        try {
          value = work.get();
        } catch (BBjAdminFailure e) {
          failure = e;
        } catch (RuntimeException e) {
          failure = createUnavailableFailure();
        }

        applyOutcome(value, failure);
      } finally {
        stopWork();
      }
    }

    void start(ThreadPoolExecutor workers, ScheduledThreadPoolExecutor timer) {
      deadline = timer.schedule(this::handleTimeout, timeout.toMillis(), TimeUnit.MILLISECONDS);
      try {
        workers.execute(this);
      } catch (RejectedExecutionException e) {
        deadline.cancel(false);
        throw e;
      }
    }

    private PendingResult<R> createFailedResult() {
      settled.set(true);
      try {
        return PendingResult.completedWith(fail.apply(createUnavailableFailure()));
      } catch (RuntimeException e) {
        sendHookWarning(e);

        return PendingResult.completedExceptionallyWith(e);
      }
    }

    private void applyOutcome(T value, BBjAdminFailure failure) {
      if (!settled.compareAndSet(false, true)) {
        if (failure == null) {
          handleDiscard(value);
        }

        return;
      }

      stopDeadline();
      if (failure != null) {
        sendToEnvironment(() -> applyFailure(failure), () -> {
        });
      } else {
        sendToEnvironment(() -> applyValue(value), () -> handleDiscard(value));
      }
    }

    private void handleTimeout() {
      if (!settled.compareAndSet(false, true)) {
        return;
      }

      synchronized (this) {
        if (worker != null && !finished) {
          abandoned = true;
          abandonedCount.incrementAndGet();
          worker.interrupt();
          logger.log(Level.WARNING, "An Admin API call missed the time limit and was abandoned");
        }
      }

      sendToEnvironment(() -> applyFailure(createUnavailableFailure()), () -> {
      });
    }

    private synchronized void stopWork() {
      finished = true;
      worker = null;
      if (abandoned) {
        abandoned = false;
        abandonedCount.decrementAndGet();
      }
    }

    private void sendToEnvironment(Runnable task, Runnable onRejected) {
      try {
        dispatcher.apply(environment, () -> {
          if (delivered.compareAndSet(false, true)) {
            task.run();
          }
        }).exceptionally(error -> {
          if (delivered.compareAndSet(false, true)) {
            onRejected.run();
          }

          return null;
        });
      } catch (RuntimeException e) {
        sendHookWarning(e);
        if (delivered.compareAndSet(false, true)) {
          onRejected.run();
        }
      }
    }

    private void applyValue(T value) {
      if (result.isCancelled()) {
        handleDiscard(value);

        return;
      }

      try {
        result.complete(deliver.apply(value));
      } catch (RuntimeException e) {
        sendHookWarning(e);
        result.completeExceptionally(e);
        handleDiscard(value);
      }
    }

    private void applyFailure(BBjAdminFailure failure) {
      if (result.isCancelled()) {
        return;
      }

      try {
        result.complete(fail.apply(failure));
      } catch (RuntimeException e) {
        sendHookWarning(e);
        result.completeExceptionally(e);
      }
    }

    private void handleDiscard(T value) {
      if (value == null) {
        return;
      }

      Runnable discarding = () -> {
        try {
          discard.accept(value);
        } catch (RuntimeException e) {
          sendHookWarning(e);
        }
      };
      if (Thread.currentThread() == worker) {
        discarding.run();
      } else {
        startDiscardThread(discarding);
      }
    }

    private void stopDeadline() {
      ScheduledFuture<?> scheduled = deadline;
      if (scheduled != null) {
        scheduled.cancel(false);
      }
    }

    private void sendHookWarning(RuntimeException e) {
      logger.log(Level.WARNING, "An Admin API task hook failed with {0}", e.getClass().getName());
    }
  }
}
