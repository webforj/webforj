package com.webforj.plugin.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;

class WatchConfigGuardTest {

  @TempDir
  Path directory;

  @Test
  @Timeout(10)
  void shouldRestoreTheFileAfterDeletion() throws Exception {
    Path file = directory.resolve("webforj.conf");
    Files.writeString(file, "webforj.devtools.livereload.enabled = true");
    List<String> notices = new CopyOnWriteArrayList<>();

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, notices::add)) {
      Files.delete(file);
      awaitFile(file);

      assertEquals("webforj.devtools.livereload.enabled = true", Files.readString(file));
      assertEquals(1, notices.size());
    }
  }

  @Test
  @Timeout(10)
  void shouldRestoreTheLastRewriteRatherThanTheStartContent() throws Exception {
    Path file = directory.resolve("webforj.conf");
    Files.writeString(file, "webforj.debug = false");

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, line -> {
    })) {
      Files.writeString(file, "webforj.debug = true");
      Files.setLastModifiedTime(file, FileTime.from(Instant.now().plusSeconds(2)));
      awaitAdoption(file);

      Files.delete(file);
      awaitFile(file);

      assertEquals("webforj.debug = true", Files.readString(file));
    }
  }

  @Test
  @Timeout(10)
  void shouldDoNothingWhenTheFileNeverExisted() throws Exception {
    Path file = directory.resolve("webforj.conf");

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, line -> {
    })) {
      Thread.sleep(WatchConfigGuard.CHECK_INTERVAL_MS * 2);

      assertFalse(Files.exists(file));
    }
  }

  @Test
  @Timeout(10)
  void shouldStopRestoringAfterClose() throws Exception {
    Path file = directory.resolve("webforj.conf");
    Files.writeString(file, "webforj.debug = true");

    WatchConfigGuard guard = WatchConfigGuard.start(file, line -> {
    });
    guard.close();

    Files.delete(file);
    Thread.sleep(WatchConfigGuard.CHECK_INTERVAL_MS * 3);

    assertFalse(Files.exists(file));
  }

  private static void awaitFile(Path file) throws InterruptedException {
    long deadline = System.currentTimeMillis() + 5000;
    while (!Files.exists(file) && System.currentTimeMillis() < deadline) {
      Thread.sleep(50);
    }

    assertTrue(Files.exists(file), "the guard did not restore " + file);
  }

  private static void awaitAdoption(Path file) throws InterruptedException, IOException {
    // The guard adopts a rewrite on its next check. Two intervals give it one guaranteed pass over
    // the new modification time.
    Thread.sleep(WatchConfigGuard.CHECK_INTERVAL_MS * 2);
    assertEquals("webforj.debug = true", Files.readString(file, StandardCharsets.UTF_8));
  }
}
