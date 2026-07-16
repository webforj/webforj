package com.webforj.plugin.foundation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.function.Consumer;

/**
 * Keeps a generated configuration file alive in the build output directory for the lifetime of the
 * watch.
 *
 * <p>
 * The build writes the webforJ configuration into the build output directory, where the application
 * reads it on every start. An IDE build compiles into the same directory and removes files it has
 * no source for, so the generated configuration can disappear between application restarts, and a
 * restart that boots without it silently loses the developer's settings. The guard runs in the
 * watch process, which outlives every application restart. It remembers the file content it saw at
 * start, adopts every rewrite, and writes the remembered content back the moment the file is gone,
 * well before the restarted application reads its configuration.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class WatchConfigGuard implements AutoCloseable {

  static final long CHECK_INTERVAL_MS = 500;

  private final Path file;
  private final Consumer<String> notice;
  private final Thread thread;
  private volatile byte[] content;
  private volatile FileTime lastModified;
  private volatile boolean running = true;

  private WatchConfigGuard(Path file, byte[] content, FileTime lastModified,
      Consumer<String> notice) {
    this.file = file;
    this.content = content;
    this.lastModified = lastModified;
    this.notice = notice;
    this.thread = new Thread(this::watch, "webforj-watch-config-guard");
    this.thread.setDaemon(true);
  }

  /**
   * Starts guarding the given file.
   *
   * <p>
   * A file that does not exist at start is not guarded, there is nothing to keep alive. The
   * returned guard then does nothing and closes harmlessly.
   * </p>
   *
   * @param file the generated configuration file to keep alive
   * @param notice receives one line whenever the guard restores the file or fails to
   * @return the running guard
   * @throws IOException when the file exists but reading it fails
   */
  public static WatchConfigGuard start(Path file, Consumer<String> notice) throws IOException {
    if (!Files.isRegularFile(file)) {
      return new WatchConfigGuard(file, null, null, notice);
    }

    WatchConfigGuard guard = new WatchConfigGuard(file, Files.readAllBytes(file),
        Files.getLastModifiedTime(file), notice);
    guard.thread.start();

    return guard;
  }

  /**
   * Stops the guard.
   */
  @Override
  public void close() {
    running = false;
    thread.interrupt();
  }

  private void watch() {
    while (running) {
      try {
        Thread.sleep(CHECK_INTERVAL_MS);
        check();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();

        return;
      }
    }
  }

  private void check() {
    if (Files.isRegularFile(file)) {
      adopt();
    } else {
      restore();
    }
  }

  /**
   * Remembers the current file content after a rewrite, so a later restore brings back what the
   * last writer put there rather than what the guard saw at start.
   */
  private void adopt() {
    try {
      FileTime modified = Files.getLastModifiedTime(file);
      if (!modified.equals(lastModified)) {
        content = Files.readAllBytes(file);
        lastModified = modified;
      }
    } catch (IOException e) {
      // A writer still holds the file, the next check adopts the settled content.
    }
  }

  private void restore() {
    try {
      Files.createDirectories(file.getParent());
      Files.write(file, content);
      lastModified = Files.getLastModifiedTime(file);
      notice.accept("restored " + file.getFileName() + " after an outside build removed it");
    } catch (IOException e) {
      notice.accept("could not restore " + file.getFileName() + ": " + e.getMessage());
    }
  }
}
