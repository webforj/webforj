package com.webforj.bbj.security;

import com.webforj.PendingResult;
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
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Runs blocking Admin API tasks off the UI thread with an overall time limit and brings the result
 * back to the Environment thread.
 *
 * <p>
 * The runner belongs to one app instance. It starts no thread before the first task. Every call of
 * {@link #start} runs on the Environment thread.
 * </p>
 *
 * <p>
 * Of the outcomes of a task exactly one wins. A task that misses the time limit is abandoned and
 * its value, when it arrives later, goes to the discard hook on the worker thread. A worker that is
 * blocked in a read cannot be woken, so a worker may stay abandoned until the server closes the
 * connection. While every worker is abandoned, a new task fails at once.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAdminTaskRunner {
  private static final Logger logger = System.getLogger(BBjAdminTaskRunner.class.getName());
  private static final int WORKER_COUNT = 2;
  private static final int QUEUE_SIZE = 4;
  private static final long KEEP_ALIVE_SECONDS = 30;

  private final Duration timeout;
  private final Supplier<Function<Runnable, PendingResult<Void>>> dispatcherSupplier;
  private final AtomicInteger abandonedCount = new AtomicInteger();
  private final AtomicInteger threadCount = new AtomicInteger();
  private ThreadPoolExecutor workers;
  private ScheduledThreadPoolExecutor timer;
  private boolean destroyed;

  /**
   * Creates a runner.
   *
   * @param timeout the overall wait for one task, queue wait included
   * @param dispatcherSupplier gives, on the Environment thread, the function that posts a task to
   *        that thread
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  BBjAdminTaskRunner(Duration timeout,
      Supplier<Function<Runnable, PendingResult<Void>>> dispatcherSupplier) {
    this.timeout = Objects.requireNonNull(timeout, "timeout");
    this.dispatcherSupplier = Objects.requireNonNull(dispatcherSupplier, "dispatcherSupplier");
  }

  /**
   * Starts a blocking task.
   *
   * <p>
   * The hooks {@code deliver} and {@code fail} run on the Environment thread and give the value of
   * the returned result. The hook {@code discard} gets a value nobody takes and never runs on the
   * Environment thread.
   * </p>
   *
   * @param <T> the type the work returns
   * @param <R> the type of the result
   * @param work the blocking work
   * @param deliver turns the value of the work into the result
   * @param fail turns a failure into the result
   * @param discard takes a value that arrived after the result was settled or cancelled
   * @return the result, completed exceptionally only when a hook throws
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  <T, R> PendingResult<R> start(Supplier<T> work, Function<T, R> deliver,
      Function<BBjAdminFailure, R> fail, Consumer<T> discard) {
    Objects.requireNonNull(work, "work");
    Objects.requireNonNull(deliver, "deliver");
    Objects.requireNonNull(fail, "fail");
    Objects.requireNonNull(discard, "discard");
    Function<Runnable, PendingResult<Void>> dispatcher = dispatcherSupplier.get();
    Task<T, R> task = new Task<>(work, deliver, fail, discard, dispatcher);
    if (!isAcceptingTasks()) {
      return task.createFailedResult();
    }

    try {
      task.start(getWorkers(), getTimer());
    } catch (RejectedExecutionException e) {
      return task.createFailedResult();
    }

    return task.result;
  }

  /**
   * Stops accepting tasks and interrupts queued and running ones. Calling it again does nothing.
   */
  void destroy() {
    synchronized (this) {
      destroyed = true;
      if (workers != null) {
        workers.shutdownNow();
      }

      if (timer != null) {
        timer.shutdownNow();
      }
    }
  }

  private synchronized boolean isAcceptingTasks() {
    return !destroyed && abandonedCount.get() < WORKER_COUNT;
  }

  private synchronized ThreadPoolExecutor getWorkers() {
    if (workers == null) {
      workers = new ThreadPoolExecutor(WORKER_COUNT, WORKER_COUNT, KEEP_ALIVE_SECONDS,
          TimeUnit.SECONDS, new ArrayBlockingQueue<>(QUEUE_SIZE),
          runnable -> createThread(runnable, "bbj-admin-" + threadCount.incrementAndGet()));
      workers.allowCoreThreadTimeOut(true);
    }

    return workers;
  }

  private synchronized ScheduledThreadPoolExecutor getTimer() {
    if (timer == null) {
      timer =
          new ScheduledThreadPoolExecutor(1, runnable -> createThread(runnable, "bbj-admin-timer"));
      timer.setKeepAliveTime(KEEP_ALIVE_SECONDS, TimeUnit.SECONDS);
      timer.allowCoreThreadTimeOut(true);
      timer.setRemoveOnCancelPolicy(true);
    }

    return timer;
  }

  private static Thread createThread(Runnable runnable, String name) {
    Thread thread = new Thread(runnable, name);
    thread.setDaemon(true);

    return thread;
  }

  private static void startDiscardThread(Runnable runnable) {
    createThread(runnable, "bbj-admin-discard").start();
  }

  private static BBjAdminFailure createUnavailableFailure() {
    return new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE,
        "The Admin API call could not be completed");
  }

  private final class Task<T, R> implements Runnable {
    private final Supplier<T> work;
    private final Function<T, R> deliver;
    private final Function<BBjAdminFailure, R> fail;
    private final Consumer<T> discard;
    private final Function<Runnable, PendingResult<Void>> dispatcher;
    private final PendingResult<R> result = new PendingResult<>();
    private final AtomicBoolean settled = new AtomicBoolean();
    private final AtomicBoolean delivered = new AtomicBoolean();
    private volatile Thread worker;
    private boolean finished;
    private boolean abandoned;
    private ScheduledFuture<?> deadline;

    Task(Supplier<T> work, Function<T, R> deliver, Function<BBjAdminFailure, R> fail,
        Consumer<T> discard, Function<Runnable, PendingResult<Void>> dispatcher) {
      this.work = work;
      this.deliver = deliver;
      this.fail = fail;
      this.discard = discard;
      this.dispatcher = dispatcher;
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

    PendingResult<R> createFailedResult() {
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
        dispatcher.apply(() -> {
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
        onRejected.run();
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
