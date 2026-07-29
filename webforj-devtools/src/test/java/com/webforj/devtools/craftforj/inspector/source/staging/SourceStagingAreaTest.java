package com.webforj.devtools.craftforj.inspector.source.staging;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SourceStagingAreaTest {

  @TempDir
  Path dir;

  private SourceStagingArea area;
  private Map<String, StagedFile> storage;

  @BeforeEach
  void setUp() {
    storage = new LinkedHashMap<>();
    area = new SourceStagingArea(() -> storage);
  }

  @Test
  @DisplayName("Should stage, list, get and discard files")
  void shouldStageListGetAndDiscard() {
    String path = dir.resolve("A.java").toString();
    area.stage(new StagedFile(path, null, "class A {}", true, true));

    assertEquals(1, area.list().size());
    assertEquals("class A {}", area.get(path).getContent());
    assertTrue(area.discard(path));
    assertTrue(area.list().isEmpty());
    assertFalse(area.discard(path));
  }

  @Test
  @DisplayName("Should replace an entry staged twice for the same path")
  void shouldReplaceEntryForSamePath() {
    String path = dir.resolve("A.java").toString();
    area.stage(new StagedFile(path, null, "class A {}", true, true));
    area.stage(new StagedFile(path, null, "class A { int x; }", true, true));

    assertEquals(1, area.list().size());
    assertEquals("class A { int x; }", area.get(path).getContent());
  }

  @Test
  @DisplayName("Should clear all staged files")
  void shouldClearAllStagedFiles() {
    area.stage(new StagedFile(dir.resolve("A.java").toString(), null, "class A {}", true, true));
    area.stage(new StagedFile(dir.resolve("B.java").toString(), null, "class B {}", true, true));
    area.clear();

    assertTrue(area.list().isEmpty());
  }

  @Test
  @DisplayName("Should apply staged files to disk and clear the area")
  void shouldApplyStagedFiles() throws IOException {
    Path existing = dir.resolve("Existing.java");
    Files.writeString(existing, "class Existing {}", StandardCharsets.UTF_8);
    String baseHash = SourceHasher.hash("class Existing {}");

    Path created = dir.resolve("pkg").resolve("Created.java");
    area.stage(
        new StagedFile(existing.toString(), baseHash, "class Existing { int x; }", false, true));
    area.stage(new StagedFile(created.toString(), null, "class Created {}", true, true));

    List<String> applied = area.apply();

    assertEquals(2, applied.size());
    assertEquals("class Existing { int x; }", Files.readString(existing, StandardCharsets.UTF_8));
    assertEquals("class Created {}", Files.readString(created, StandardCharsets.UTF_8));
    assertTrue(area.list().isEmpty());
  }

  @Test
  @DisplayName("Should refuse to apply when a file changed on disk after it was read")
  void shouldRefuseApplyOnStaleHash() throws IOException {
    Path existing = dir.resolve("Stale.java");
    Files.writeString(existing, "class Stale {}", StandardCharsets.UTF_8);
    String baseHash = SourceHasher.hash("class Stale {}");
    area.stage(
        new StagedFile(existing.toString(), baseHash, "class Stale { int x; }", false, true));

    Files.writeString(existing, "class Stale { int edited; }", StandardCharsets.UTF_8);

    StagingException ex = assertThrows(StagingException.class, () -> area.apply());
    assertEquals("SOURCE_CHANGED", ex.getCode());
    assertEquals("class Stale { int edited; }", Files.readString(existing, StandardCharsets.UTF_8));
    assertEquals(1, area.list().size());
  }

  @Test
  @DisplayName("Should restore every pre image when a write fails midway")
  void shouldRestoreOnMidWriteFailure() throws IOException {
    Path existing = dir.resolve("First.java");
    Files.writeString(existing, "class First {}", StandardCharsets.UTF_8);
    String baseHash = SourceHasher.hash("class First {}");
    area.stage(
        new StagedFile(existing.toString(), baseHash, "class First { int x; }", false, true));

    Path blocker = dir.resolve("blocker");
    Files.writeString(blocker, "not a directory", StandardCharsets.UTF_8);
    Path impossible = blocker.resolve("nested").resolve("Second.java");
    area.stage(new StagedFile(impossible.toString(), null, "class Second {}", true, true));

    StagingException ex = assertThrows(StagingException.class, () -> area.apply());

    assertEquals("APPLY_FAILED", ex.getCode());
    assertEquals("class First {}", Files.readString(existing, StandardCharsets.UTF_8));
    assertFalse(Files.exists(impossible));
    assertEquals(2, area.list().size());
  }

  @Test
  @DisplayName("Should return an empty list when nothing is staged")
  void shouldReturnEmptyListWhenNothingStaged() {
    assertTrue(area.apply().isEmpty());
  }

  @Test
  @DisplayName("Should return null for a file that is not staged")
  void shouldReturnNullForUnstagedFile() {
    assertNull(area.get(dir.resolve("Missing.java").toString()));
  }
}
