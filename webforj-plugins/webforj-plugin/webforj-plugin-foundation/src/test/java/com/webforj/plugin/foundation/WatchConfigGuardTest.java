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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;

class WatchConfigGuardTest {

  private static final String CONFIG_FILE = "webforj.conf";
  private static final String DEBUG_LINE = "webforj.debug = true";

  @TempDir
  Path directory;

  @Test
  @Timeout(10)
  @SuppressWarnings("try")
  void shouldRestoreTheFileAfterDeletion() throws Exception {
    Path file = directory.resolve(CONFIG_FILE);
    Files.writeString(file, "webforj.devtools.livereload.enabled = true");
    List<String> notices = new CopyOnWriteArrayList<>();
    CountDownLatch restored = new CountDownLatch(1);

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, line -> {
      notices.add(line);
      restored.countDown();
    })) {
      Files.delete(file);

      assertEquals("webforj.devtools.livereload.enabled = true", readRestoredFile(restored, file));
      assertEquals(1, notices.size());
    }
  }

  @Test
  @Timeout(10)
  @SuppressWarnings("try")
  void shouldRestoreTheLastRewriteRatherThanTheStartContent() throws Exception {
    Path file = directory.resolve(CONFIG_FILE);
    Files.writeString(file, "webforj.debug = false");
    CountDownLatch restored = new CountDownLatch(1);

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, line -> restored.countDown())) {
      Files.writeString(file, DEBUG_LINE);
      Files.setLastModifiedTime(file, FileTime.from(Instant.now().plusSeconds(2)));
      assertFalse(restored.await(WatchConfigGuard.CHECK_INTERVAL_MS * 2, TimeUnit.MILLISECONDS));
      assertEquals(DEBUG_LINE, Files.readString(file, StandardCharsets.UTF_8));

      Files.delete(file);

      assertEquals(DEBUG_LINE, readRestoredFile(restored, file));
    }
  }

  @Test
  @Timeout(10)
  @SuppressWarnings("try")
  void shouldDoNothingWhenTheFileNeverExisted() throws Exception {
    Path file = directory.resolve(CONFIG_FILE);
    CountDownLatch restored = new CountDownLatch(1);

    try (WatchConfigGuard guard = WatchConfigGuard.start(file, line -> restored.countDown())) {
      assertFalse(restored.await(WatchConfigGuard.CHECK_INTERVAL_MS * 2, TimeUnit.MILLISECONDS));

      assertFalse(Files.exists(file));
    }
  }

  @Test
  @Timeout(10)
  void shouldStopRestoringAfterClose() throws Exception {
    Path file = directory.resolve(CONFIG_FILE);
    Files.writeString(file, DEBUG_LINE);
    CountDownLatch restored = new CountDownLatch(1);

    WatchConfigGuard guard = WatchConfigGuard.start(file, line -> restored.countDown());
    guard.close();

    Files.delete(file);
    assertFalse(restored.await(WatchConfigGuard.CHECK_INTERVAL_MS * 3, TimeUnit.MILLISECONDS));

    assertFalse(Files.exists(file));
  }

  private static String readRestoredFile(CountDownLatch restored, Path file)
      throws InterruptedException, IOException {
    assertTrue(restored.await(5, TimeUnit.SECONDS), "the guard did not restore " + file);
    assertTrue(Files.exists(file), "the guard did not restore " + file);

    return Files.readString(file);
  }
}
