package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class GetStagedSourceActionTest {

  @TempDir
  Path dir;

  private final Map<String, StagedFile> storage = new LinkedHashMap<>();
  private SourceStagingArea stagingArea;
  private GetStagedSourceAction action;

  @BeforeEach
  void setUp() {
    stagingArea = new SourceStagingArea(() -> storage);
    action = new GetStagedSourceAction(stagingArea);
  }

  @Test
  @DisplayName("Should list staged files with original and patched content")
  void shouldListStagedFilesWithContentPair() throws IOException {
    Path existing = dir.resolve("Edited.java");
    Files.writeString(existing, "class Edited {}", StandardCharsets.UTF_8);
    stagingArea
        .stage(new StagedFile(existing.toString(), "hash", "class Edited { int x; }", false, true));

    Path created = dir.resolve("Created.java");
    stagingArea.stage(new StagedFile(created.toString(), null, "class Created {}", true, false));

    GetStagedSourceAction.Response response = action.handle(new JsonObject());

    assertEquals(2, response.getFiles().size());

    GetStagedSourceAction.StagedFileView edited = response.getFiles().get(0);
    assertEquals(existing.toString(), edited.getPath());
    assertFalse(edited.isNewFile());
    assertTrue(edited.isVerified());
    assertEquals("class Edited {}", edited.getOriginal());
    assertEquals("class Edited { int x; }", edited.getPatched());

    GetStagedSourceAction.StagedFileView freshFile = response.getFiles().get(1);
    assertTrue(freshFile.isNewFile());
    assertFalse(freshFile.isVerified());
    assertEquals("", freshFile.getOriginal());
    assertEquals("class Created {}", freshFile.getPatched());
  }

  @Test
  @DisplayName("Should return an empty list when nothing is staged")
  void shouldReturnEmptyListWhenNothingStaged() {
    GetStagedSourceAction.Response response = action.handle(new JsonObject());

    assertTrue(response.getFiles().isEmpty());
  }
}
