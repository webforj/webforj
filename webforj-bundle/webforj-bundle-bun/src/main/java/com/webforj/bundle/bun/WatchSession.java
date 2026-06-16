package com.webforj.bundle.bun;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * A running development watch.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class WatchSession implements AutoCloseable {

  /**
   * The operations a watch session calls back into its host to keep the bundle current across
   * development restarts.
   */
  public interface WatchHost {

    /**
     * Computes the current set of bundle entries, compared against the running set to decide
     * whether a restart changed what the watcher must build.
     *
     * @return the current set of bundle entries
     */
    BundleEntrySet currentEntrySet();

    /**
     * Syncs the development bundle output, the served files and the index, so it is current for the
     * restarted application.
     *
     * @throws IOException on any IO failure
     */
    void syncOutput() throws IOException;

    /**
     * Rebuilds the bundle and starts a fresh watcher on the changed entry set.
     *
     * @return the new watcher process, or {@code null} when there is nothing left to bundle
     * @throws IOException on any IO failure
     * @throws InterruptedException if the rebuild is interrupted
     */
    Process restartWatcher() throws IOException, InterruptedException;
  }

  private final WatchHost host;
  private final BundleLogger log;
  private final ExecutorService rescans = Executors.newSingleThreadExecutor(runnable -> {
    Thread thread = new Thread(runnable, "webforj-watch-rescan");
    thread.setDaemon(true);

    return thread;
  });

  private volatile Process watcher;
  private volatile BundleEntrySet entrySet;
  private volatile boolean closed;

  WatchSession(Process watcher, WatchHost host, BundleLogger log) {
    this.watcher = watcher;
    this.host = host;
    this.entrySet = host.currentEntrySet();
    this.log = log;
  }

  /**
   * Rescans for a changed set of bundle entries and rebuilds when it changed.
   */
  public void rescan() {
    if (!closed) {
      rescans.submit(this::run);
    }
  }

  private void run() {
    if (closed) {
      return;
    }

    try {
      // A recompile can wipe the served bundle files and the index out of the output directory, so
      // sync them on every restart. This keeps the running set both served and resolvable for the
      // restarted application even when the entries are unchanged and nothing is rebuilt.
      host.syncOutput();

      BundleEntrySet current = host.currentEntrySet();
      // The restart left the entries untouched, so the running watcher already builds the right
      // set. Nothing more to rebuild.
      if (current.equals(entrySet)) {
        return;
      }

      log.info("bundle entries changed, rebuilding the development bundle");
      Process previous = watcher;
      if (previous != null) {
        previous.destroy();
      }

      watcher = host.restartWatcher();
      // Advance the baseline when the rebuild produced a watcher, or when there are no entries left
      // to build at all, so a removed entry settles to the empty state and a re-added one rebuilds.
      // A non empty set that still produced nothing has a source not in place yet, so the baseline
      // stays for the next restart to retry once the source is back.
      if (watcher != null || current.isEmpty()) {
        entrySet = current;
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    } catch (IOException | RuntimeException e) {
      log.warn("failed to sync the development bundle after a restart: {}", e.getMessage());
    }
  }

  /**
   * Stops the rescans and the running watcher.
   */
  @Override
  public void close() {
    closed = true;
    rescans.shutdownNow();
    Process current = watcher;

    if (current != null) {
      current.destroy();
    }
  }
}
