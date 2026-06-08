package com.webforj.bundle.bun;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bundle.BundleIndex;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundlerExecutionSyncTest {

  @Test
  void shouldAddDebugEntryOutputsUnderTheReservedKey() {
    Map<String, List<String>> keyToFiles = new LinkedHashMap<>();
    keyToFiles.put("card/card.ts", List.of("card/card-A1.js"));
    keyToFiles.put("panel/panel.ts", List.of("panel/panel-D3.js", "panel/panel-E4.css"));

    Map<String, List<String>> target = new LinkedHashMap<>();
    target.put("CardView", List.of("card/card-A1.js"));

    BundlerExecution.addDebugFiles(target, keyToFiles, Set.of("panel/panel.ts"));

    assertEquals(List.of("panel/panel-D3.js", "panel/panel-E4.css"),
        target.get(BundleIndex.DEBUG_KEY));
  }

  @Test
  void shouldAddNoDebugKeyWhenNoDebugSources() {
    Map<String, List<String>> target = new LinkedHashMap<>();
    target.put("CardView", List.of("card/card-A1.js"));

    BundlerExecution.addDebugFiles(target, Map.of("card/card.ts", List.of("card/card-A1.js")),
        Set.of());

    assertFalse(target.containsKey(BundleIndex.DEBUG_KEY));
  }

  @Test
  void shouldReportOnlyChangedAndNewFiles(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.writeString(from.resolve("card.js"), "same");
    Files.writeString(to.resolve("card.js"), "same");
    Files.writeString(from.resolve("card.css"), "new-styles");
    Files.writeString(to.resolve("card.css"), "old-styles");
    Files.writeString(from.resolve("counter.js"), "fresh");

    List<String> changed = BundlerExecution.syncChangedFiles(from, to);

    assertEquals(List.of("card.css", "counter.js"), changed.stream().sorted().toList(),
        "only the changed card.css and the new counter.js");
    assertEquals("new-styles", Files.readString(to.resolve("card.css")));
    assertEquals("fresh", Files.readString(to.resolve("counter.js")));
  }

  @Test
  void shouldReportNestedPathsWithForwardSlashes(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.createDirectories(from.resolve("styles"));
    Files.writeString(from.resolve("styles").resolve("theme.css"), "body{}");

    List<String> changed = BundlerExecution.syncChangedFiles(from, to);

    assertEquals(List.of("styles/theme.css"), changed);
  }

  @Test
  void shouldKeepANestedServedFileThatIsStillBuilt(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.createDirectories(from.resolve("styles"));
    Files.createDirectories(to.resolve("styles"));
    Files.writeString(from.resolve("styles").resolve("theme.css"), "body{}");
    Files.writeString(to.resolve("styles").resolve("theme.css"), "body{}");

    List<String> changed = BundlerExecution.syncChangedFiles(from, to);

    assertTrue(changed.isEmpty(), "an unchanged nested file is neither reported nor deleted");
    assertTrue(Files.exists(to.resolve("styles").resolve("theme.css")));
  }

  @Test
  void shouldDeleteANestedStaleServedFile(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.createDirectories(to.resolve("styles"));
    Files.writeString(to.resolve("styles").resolve("old.css"), "stale");

    List<String> changed = BundlerExecution.syncChangedFiles(from, to);

    assertEquals(List.of("styles/old.css"), changed);
    assertFalse(Files.exists(to.resolve("styles").resolve("old.css")));
  }

  @Test
  void shouldReportDeletedServedFilesNoLongerBuilt(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.writeString(from.resolve("card.js"), "x");
    Files.writeString(to.resolve("card.js"), "x");
    Files.writeString(to.resolve("removed.js"), "stale");

    List<String> changed = BundlerExecution.syncChangedFiles(from, to);

    assertEquals(List.of("removed.js"), changed, "only the removed file counts as a change");
    assertFalse(Files.exists(to.resolve("removed.js")));
    assertTrue(Files.exists(to.resolve("card.js")));
  }

  @Test
  void shouldReportNothingWhenIdentical(@TempDir Path tmp) throws IOException {
    Path from = Files.createDirectories(tmp.resolve("staging"));
    Path to = Files.createDirectories(tmp.resolve("served"));
    Files.writeString(from.resolve("a.js"), "one");
    Files.writeString(to.resolve("a.js"), "one");

    assertTrue(BundlerExecution.syncChangedFiles(from, to).isEmpty());
  }

  @Test
  void shouldReportNothingWhenStagingMissing(@TempDir Path tmp) throws IOException {
    Path to = Files.createDirectories(tmp.resolve("served"));

    assertTrue(BundlerExecution.syncChangedFiles(tmp.resolve("absent"), to).isEmpty());
  }
}
