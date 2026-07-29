package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DiscardStagedSourceActionTest {

  @TempDir
  Path dir;

  private final Map<String, StagedFile> storage = new LinkedHashMap<>();
  private SourceStagingArea stagingArea;
  private DiscardStagedSourceAction action;

  @BeforeEach
  void setUp() {
    stagingArea = new SourceStagingArea(() -> storage);
    action = new DiscardStagedSourceAction(stagingArea);
    stagingArea
        .stage(new StagedFile(dir.resolve("A.java").toString(), null, "class A {}", true, true));
    stagingArea
        .stage(new StagedFile(dir.resolve("B.java").toString(), null, "class B {}", true, true));
  }

  @Test
  @DisplayName("Should discard a single staged file by path")
  void shouldDiscardSingleFile() {
    JsonObject params = new JsonObject();
    params.addProperty("path", dir.resolve("A.java").toString());

    DiscardStagedSourceAction.Response response = action.handle(params);

    assertTrue(response.isDiscarded());
    assertEquals(1, stagingArea.list().size());
  }

  @Test
  @DisplayName("Should discard everything when no path is given")
  void shouldDiscardEverythingWithoutPath() {
    DiscardStagedSourceAction.Response response = action.handle(new JsonObject());

    assertTrue(response.isDiscarded());
    assertTrue(stagingArea.list().isEmpty());
  }

  @Test
  @DisplayName("Should report false for a path that is not staged")
  void shouldReportFalseForUnstagedPath() {
    JsonObject params = new JsonObject();
    params.addProperty("path", dir.resolve("Missing.java").toString());

    DiscardStagedSourceAction.Response response = action.handle(params);

    assertFalse(response.isDiscarded());
    assertEquals(2, stagingArea.list().size());
  }
}
