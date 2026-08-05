package com.webforj.devtools.livereload.receiver;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Collapses a burst of class changes into one debounced browser update through the
 * {@link LiveReloadServer}, sending the batched class names or a full page reload when a change is
 * unnamed.
 *
 * <p>
 * Every hotswap receiver owns one delivery and feeds it the changes its tool reports. The delivery
 * runs the debounce on its own daemon thread between a start and a stop, and a change arriving
 * outside that window is ignored.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class ClassUpdateDelivery {

  private static final System.Logger logger = System.getLogger(ClassUpdateDelivery.class.getName());
  private static final long DEBOUNCE_DELAY_MILLIS = 150;

  private final LiveReloadServer server;
  private final String description;
  private final String threadName;
  private final AtomicReference<ScheduledFuture<?>> pendingReload = new AtomicReference<>();
  private final Set<String> pendingClasses = ConcurrentHashMap.newKeySet();
  private final AtomicBoolean sawUnnamedChange = new AtomicBoolean(false);

  private volatile ScheduledExecutorService executor;

  /**
   * Creates a delivery that pushes updates through the given reload server.
   *
   * @param server the reload server the update is pushed through
   * @param description the change description the log lines carry, such as a JRebel class reload
   * @param threadName the name of the debounce thread
   */
  ClassUpdateDelivery(LiveReloadServer server, String description, String threadName) {
    this.server = server;
    this.description = description;
    this.threadName = threadName;
  }

  /**
   * Starts the debounce thread, so changes are accepted.
   */
  void start() {
    executor = Executors.newSingleThreadScheduledExecutor(this::newDaemonThread);
  }

  /**
   * Cancels a pending update, drops the batched changes, and stops the debounce thread.
   */
  void stop() {
    final ScheduledExecutorService current = executor;
    executor = null;

    ScheduledFuture<?> scheduled = pendingReload.getAndSet(null);
    if (scheduled != null) {
      scheduled.cancel(false);
    }

    pendingClasses.clear();
    sawUnnamedChange.set(false);

    if (current != null) {
      current.shutdownNow();
    }
  }

  /**
   * Indicates whether the delivery accepts changes.
   *
   * @return {@code true} between a start and a stop
   */
  boolean isRunning() {
    return executor != null;
  }

  /**
   * Accepts one changed class and schedules the debounced update.
   *
   * @param className the binary name of the changed class
   */
  void classChanged(String className) {
    ScheduledExecutorService current = executor;
    if (current == null) {
      return;
    }

    pendingClasses.add(className);
    schedule(current);
  }

  /**
   * Accepts one change that names no class, so the batch falls back to the full page reload, and
   * schedules the debounced update.
   */
  void unnamedChange() {
    ScheduledExecutorService current = executor;
    if (current == null) {
      return;
    }

    sawUnnamedChange.set(true);
    schedule(current);
  }

  void deliver() {
    pendingReload.set(null);

    // The batch is drained before the server check, so a batch that cannot be delivered never
    // leaks into a later one.
    Set<String> changedClasses = new TreeSet<>(pendingClasses);
    pendingClasses.clear();
    boolean unnamed = sawUnnamedChange.getAndSet(false);

    // Every schedule follows a recorded change, so an empty batch without the unnamed flag only
    // means an earlier drain already delivered these classes and nothing is left to send.
    if (changedClasses.isEmpty() && !unnamed) {
      return;
    }

    if (server == null || !server.isRunning()) {
      return;
    }

    // The reload server logs the one line per broadcast, so this stays quiet at the info level.
    if (unnamed) {
      logger.log(System.Logger.Level.DEBUG, "Triggering browser reload for " + description);
      server.sendReloadMessage();
      return;
    }

    logger.log(System.Logger.Level.DEBUG,
        "Sending a class update for " + description + ": " + String.join(", ", changedClasses));
    server.sendClassUpdateMessage(changedClasses);
  }

  private void schedule(ScheduledExecutorService current) {
    try {
      ScheduledFuture<?> previous = pendingReload
          .getAndSet(current.schedule(this::deliver, DEBOUNCE_DELAY_MILLIS, TimeUnit.MILLISECONDS));

      if (previous != null) {
        previous.cancel(false);
      }
    } catch (RuntimeException e) {
      // A change racing the stop can reach an executor that is already shut down, and the change
      // it carries dies with the receiver.
      logger.log(System.Logger.Level.DEBUG,
          "Could not schedule a browser update for " + description, e);
    }
  }

  private Thread newDaemonThread(Runnable task) {
    Thread thread = new Thread(task, threadName);
    thread.setDaemon(true);

    return thread;
  }
}
