package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.bbjship.foundation.PollingWait;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class DeployerLockTimeoutTest {

  @TempDir
  Path root;

  @Test
  void shouldFailWithoutTheLockOnceTheLockTimeoutPassed() throws Exception {
    AtomicLong now = new AtomicLong();
    List<Duration> pauses = new ArrayList<>();
    PollingWait wait = PollingWait.create("Waiting", Deployer.LOCK_TIMEOUT, Duration.ofSeconds(10))
        .setClock(now::get).setSleeper(pause -> {
          pauses.add(pause);
          now.addAndGet(pause.toNanos());
        }).build();

    try (FileChannel holder = FileChannel.open(root.resolve(".lock"), StandardOpenOption.CREATE,
        StandardOpenOption.WRITE)) {
      FileLock held = holder.lock();

      IOException failure = assertThrows(IOException.class, () -> Deployer.openLock(root, wait));

      assertEquals("Another deployment is in progress", failure.getMessage());
      assertEquals(3, pauses.size());
      held.release();
    }

    try (FileChannel channel = Deployer.openLock(root, wait)) {
      assertNotNull(channel);
    }
  }
}
