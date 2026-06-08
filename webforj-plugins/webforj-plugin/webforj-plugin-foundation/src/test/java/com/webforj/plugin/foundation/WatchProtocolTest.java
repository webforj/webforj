package com.webforj.plugin.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.junit.jupiter.api.Test;

class WatchProtocolTest {

  @Test
  void shouldFormatALogLineWithThePrefix() {
    String line = WatchProtocol.log("bundling main.js");

    assertTrue(line.startsWith(WatchProtocol.LOG_PREFIX));
    assertEquals("bundling main.js", line.substring(WatchProtocol.LOG_PREFIX.length()));
  }

  @Test
  void shouldFormatAWarnLineWithThePrefix() {
    String line = WatchProtocol.warn("duplicate package");

    assertTrue(line.startsWith(WatchProtocol.WARN_PREFIX));
    assertEquals("duplicate package", line.substring(WatchProtocol.WARN_PREFIX.length()));
  }

  @Test
  void shouldFormatARebuildLineWithSpaceSeparatedPaths() {
    String line = WatchProtocol.rebuild(List.of("app.css", "main.js"));

    assertTrue(line.startsWith(WatchProtocol.REBUILD_PREFIX));
    assertEquals("app.css main.js", line.substring(WatchProtocol.REBUILD_PREFIX.length()));
  }
}
