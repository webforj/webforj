package com.webforj.plugin.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import org.junit.jupiter.api.Test;

class WatchPortFileTest {

  @Test
  void shouldResolveUnderTempDirectoryWithExpectedName() {
    Path file = WatchPortFile.resolve("/home/dev/project");
    Path tmp = Path.of(System.getProperty("java.io.tmpdir"));

    assertEquals(tmp.toAbsolutePath().normalize(), file.getParent().toAbsolutePath().normalize());
    assertTrue(file.getFileName().toString().startsWith("webforj-watch-"));
    assertTrue(file.getFileName().toString().endsWith(".port"));
  }

  @Test
  void shouldResolveDeterministicallyForTheSamePath() {
    assertEquals(WatchPortFile.resolve("/home/dev/project"),
        WatchPortFile.resolve("/home/dev/project"));
  }

  @Test
  void shouldResolveDifferentFilesForDifferentProjects() {
    assertNotEquals(WatchPortFile.resolve("/home/dev/project-a"),
        WatchPortFile.resolve("/home/dev/project-b"));
  }
}
