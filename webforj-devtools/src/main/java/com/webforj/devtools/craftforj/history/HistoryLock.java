package com.webforj.devtools.craftforj.history;

import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.channels.FileChannel;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The lock of one journal directory, held by one call at a time across threads and servers.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class HistoryLock {

  private static final Logger LOGGER = System.getLogger(HistoryLock.class.getName());
  private static final Map<Path, Folder> FOLDERS = new ConcurrentHashMap<>();
  private static final String LOCK_DIRECTORY = "lock";
  private static final String FILE = "journal.lock";
  private static final long RETRY_MILLIS = 5;

  private final Path directory;
  private final Folder folder;

  HistoryLock(Path directory) {
    this.directory = directory;
    this.folder =
        FOLDERS.computeIfAbsent(directory.toAbsolutePath().normalize(), key -> new Folder());
  }

  /**
   * Creates a hold on the journal, waiting for another holder up to the timeout.
   *
   * @throws HistoryBusyException when the journal is still held after the timeout, or the calling
   *         thread holds it already
   * @throws IOException when the lock file cannot be opened
   */
  Hold createHold(long timeout) throws IOException {
    if (folder.isHeldByCurrentThread()) {
      throw new HistoryBusyException();
    }

    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeout);
    try {
      if (!folder.tryLock(timeout, TimeUnit.MILLISECONDS)) {
        throw new HistoryBusyException();
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new IOException("Interrupted while waiting for the history", e);
    }

    FileChannel channel = null;
    try {
      Path lockDirectory = directory.resolve(LOCK_DIRECTORY);
      Files.createDirectories(lockDirectory);
      channel = FileChannel.open(lockDirectory.resolve(FILE), StandardOpenOption.CREATE,
          StandardOpenOption.WRITE);
      createFileLock(channel, deadline);

      return new Hold(folder, channel);
    } catch (IOException | RuntimeException e) {
      new Hold(folder, channel).close();
      throw e;
    }
  }

  boolean isCleaned() {
    return folder.cleaned;
  }

  void setCleaned(boolean cleaned) {
    folder.cleaned = cleaned;
  }

  private static void createFileLock(FileChannel channel, long deadline) throws IOException {
    while (true) {
      try {
        if (channel.tryLock() != null) {
          return;
        }
      } catch (OverlappingFileLockException e) {
        LOGGER.log(Level.DEBUG, "Another class loader of this server holds the history");
      }

      if (System.nanoTime() - deadline > 0) {
        throw new HistoryBusyException();
      }

      try {
        Thread.sleep(RETRY_MILLIS);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new IOException("Interrupted while waiting for the history", e);
      }
    }
  }

  /**
   * The hold of one call on the journal, released on close.
   *
   * @since 26.03
   */
  static final class Hold implements AutoCloseable {

    private final ReentrantLock lock;
    private final FileChannel channel;

    private Hold(ReentrantLock lock, FileChannel channel) {
      this.lock = lock;
      this.channel = channel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
      try {
        if (channel != null) {
          channel.close();
        }
      } catch (IOException e) {
        LOGGER.log(Level.WARNING, "Could not release the history lock", e);
      } finally {
        lock.unlock();
      }
    }
  }

  private static final class Folder extends ReentrantLock {

    private static final long serialVersionUID = 1L;

    private volatile boolean cleaned;
  }
}
