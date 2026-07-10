package com.webforj.bundle.bun;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import org.junit.jupiter.api.Test;

class WatchSessionTest {

  @Test
  void shouldSyncTheOutputButNotRebuildWhenTheEntrySetIsUnchanged() throws InterruptedException {
    FakeProcess running = new FakeProcess();
    CountDownLatch synced = new CountDownLatch(1);
    CountDownLatch rebuilt = new CountDownLatch(1);
    WatchSession session =
        new WatchSession(running, host(() -> entrySet("a"), synced::countDown, () -> {
          rebuilt.countDown();

          return new FakeProcess();
        }), BundleLogger.system());

    session.rescan();

    // A restart always syncs the served output, since a recompile can wipe it, even when the
    // entries are unchanged and nothing is rebuilt.
    assertTrue(synced.await(1, TimeUnit.SECONDS), "the output is synced on every restart");
    assertFalse(rebuilt.await(300, TimeUnit.MILLISECONDS),
        "an unchanged entry set does not rebuild");
    assertFalse(running.isDestroyed(), "the running watcher keeps going");

    session.close();
  }

  @Test
  void shouldRebuildAndReplaceTheWatcherWhenTheEntrySetChanged() throws InterruptedException {
    FakeProcess first = new FakeProcess();
    FakeProcess second = new FakeProcess();
    CountDownLatch rebuilt = new CountDownLatch(1);
    AtomicReference<BundleEntrySet> live = new AtomicReference<>(entrySet("a"));
    WatchSession session = new WatchSession(first, host(live::get, () -> {
    }, () -> {
      rebuilt.countDown();

      return second;
    }), BundleLogger.system());

    live.set(entrySet("a", "b"));
    session.rescan();

    assertTrue(rebuilt.await(2, TimeUnit.SECONDS), "a changed entry set rebuilds");
    assertTrue(first.awaitDestroyed(2, TimeUnit.SECONDS), "the old watcher is stopped");

    session.close();

    assertTrue(second.awaitDestroyed(2, TimeUnit.SECONDS), "closing stops the new watcher");
  }

  @Test
  void shouldRetryOnTheNextRestartWhenARebuildProducesNothing() throws InterruptedException {
    FakeProcess running = new FakeProcess();
    AtomicReference<BundleEntrySet> live = new AtomicReference<>(entrySet("a"));
    CountDownLatch rebuilds = new CountDownLatch(2);
    WatchSession session = new WatchSession(running, host(live::get, () -> {
    }, () -> {
      rebuilds.countDown();

      return null;
    }), BundleLogger.system());

    live.set(entrySet("a", "b"));
    session.rescan();
    session.rescan();

    assertTrue(rebuilds.await(2, TimeUnit.SECONDS),
        "an unbuildable entry set is retried on the next restart");

    session.close();
  }

  @Test
  void shouldRebuildWhenAnEntrySetReturnsAfterBecomingEmpty() throws InterruptedException {
    FakeProcess running = new FakeProcess();
    // The scan returns the baseline, then the empty set when the entry is removed, then the entry
    // again when it is added back. The queue is drained once per scan, so the order is fixed.
    Queue<BundleEntrySet> scans =
        new ConcurrentLinkedQueue<>(List.of(entrySet("a"), entrySet(), entrySet("a")));
    AtomicReference<BundleEntrySet> last = new AtomicReference<>(entrySet("a"));
    CountDownLatch rebuilt = new CountDownLatch(1);
    WatchSession session = new WatchSession(running, host(() -> {
      BundleEntrySet next = scans.poll();
      last.set(next != null ? next : last.get());

      return last.get();
    }, () -> {
    }, () -> {
      if (last.get().isEmpty()) {
        return null;
      }

      rebuilt.countDown();

      return new FakeProcess();
    }), BundleLogger.system());

    session.rescan();
    session.rescan();

    assertTrue(rebuilt.await(2, TimeUnit.SECONDS),
        "a re-added entry set rebuilds after the set went empty");

    session.close();
  }

  @Test
  void shouldStopTheWatcherOnClose() {
    FakeProcess running = new FakeProcess();
    WatchSession session = new WatchSession(running, host(() -> entrySet("a"), () -> {
    }, () -> null), BundleLogger.system());

    session.close();

    assertTrue(running.isDestroyed());
  }

  @Test
  void shouldIgnoreARescanAfterClose() throws InterruptedException {
    FakeProcess running = new FakeProcess();
    CountDownLatch rebuilt = new CountDownLatch(1);
    AtomicReference<BundleEntrySet> live = new AtomicReference<>(entrySet("a"));
    WatchSession session = new WatchSession(running, host(live::get, () -> {
    }, () -> {
      rebuilt.countDown();

      return new FakeProcess();
    }), BundleLogger.system());

    live.set(entrySet("a", "b"));
    session.close();
    session.rescan();

    assertFalse(rebuilt.await(300, TimeUnit.MILLISECONDS), "a closed session does not rebuild");
  }

  private static WatchSession.WatchHost host(Supplier<BundleEntrySet> entries, Runnable sync,
      Restart restart) {
    return new WatchSession.WatchHost() {
      @Override
      public BundleEntrySet currentEntrySet() {
        return entries.get();
      }

      @Override
      public void syncOutput() {
        sync.run();
      }

      @Override
      public Process restartWatcher() throws IOException, InterruptedException {
        return restart.get();
      }
    };
  }

  private static BundleEntrySet entrySet(String... entries) {
    return new BundleEntrySet(Set.of(entries), Set.of(), Set.of(), Map.of());
  }

  @FunctionalInterface
  private interface Restart {
    Process get() throws IOException, InterruptedException;
  }

  private static final class FakeProcess extends Process {

    private final CountDownLatch destroyed = new CountDownLatch(1);

    @Override
    public OutputStream getOutputStream() {
      return OutputStream.nullOutputStream();
    }

    @Override
    public InputStream getInputStream() {
      return InputStream.nullInputStream();
    }

    @Override
    public InputStream getErrorStream() {
      return InputStream.nullInputStream();
    }

    @Override
    public int waitFor() {
      return 0;
    }

    @Override
    public int exitValue() {
      return 0;
    }

    @Override
    public void destroy() {
      destroyed.countDown();
    }

    private boolean isDestroyed() {
      return destroyed.getCount() == 0;
    }

    private boolean awaitDestroyed(long timeout, TimeUnit unit) throws InterruptedException {
      return destroyed.await(timeout, unit);
    }
  }
}
