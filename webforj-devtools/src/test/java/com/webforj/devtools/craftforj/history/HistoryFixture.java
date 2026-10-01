package com.webforj.devtools.craftforj.history;

import com.google.gson.Gson;
import com.webforj.devtools.craftforj.history.model.HistoryEntry;
import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.history.model.HistoryIndex;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.channels.FileChannel;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.function.Supplier;
import java.util.stream.Stream;

final class HistoryFixture {

  private HistoryFixture() {}

  static byte[] toBytes(String content) {
    return content.getBytes(StandardCharsets.UTF_8);
  }

  static void write(Path file, byte[] content) {
    try {
      Files.write(file, content);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  static Long addStep(HistoryJournal journal, List<Path> files, Supplier<?> write) {
    HistoryStep step = journal.openStep();
    try {
      files.forEach(step::addFile);
      write.get();
    } catch (RuntimeException | Error e) {
      step.close();
      throw e;
    }

    return step.close();
  }

  static void addWrite(HistoryJournal journal, Path file, byte[] content) {
    addStep(journal, List.of(file), () -> {
      write(file, content);
      return null;
    });
  }

  static void addWrite(HistoryJournal journal, Path file, byte[] content, CountDownLatch holding,
      CountDownLatch release) {
    addStep(journal, List.of(file), () -> {
      write(file, content);
      holding.countDown();
      try {
        release.await();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
      return null;
    });
  }

  static List<String> getPaths(HistoryEntryInfo entry) {
    return entry.getFiles().stream().map(HistoryFileSnapshot::getPath).toList();
  }

  static List<HistoryEntry> readEntries(Path store) throws IOException {
    return new Gson().fromJson(Files.readString(store.resolve("journal.json")), HistoryIndex.class)
        .getEntries();
  }

  static long getSnapshotCount(Path store) throws IOException {
    Path snapshots = store.resolve("snapshots");
    if (!Files.isDirectory(snapshots)) {
      return 0;
    }

    try (Stream<Path> files = Files.list(snapshots)) {
      return files.count();
    }
  }

  static Path getLockFile(Path store) {
    return store.resolve("lock/journal.lock");
  }

  static boolean isLockFree(Path store) throws IOException {
    try (FileChannel channel = FileChannel.open(getLockFile(store), StandardOpenOption.WRITE)) {
      return channel.tryLock() != null;
    } catch (OverlappingFileLockException e) {
      return false;
    }
  }
}
