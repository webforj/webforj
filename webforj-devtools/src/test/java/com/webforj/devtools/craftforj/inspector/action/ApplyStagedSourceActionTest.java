package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceHasher;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

class ApplyStagedSourceActionTest {

  @TempDir
  Path dir;

  private MockedStatic<SourcePathRegistry> registryMock;
  private final Map<String, StagedFile> storage = new LinkedHashMap<>();
  private SourceStagingArea stagingArea;
  private ApplyStagedSourceAction action;

  @BeforeEach
  void setUp() {
    registryMock = mockStatic(SourcePathRegistry.class);
    registryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);
    stagingArea = new SourceStagingArea(() -> storage);
    action = new ApplyStagedSourceAction(stagingArea);
  }

  @AfterEach
  void tearDown() {
    registryMock.close();
  }

  @Test
  @DisplayName("Should write staged files and record the new paths")
  void shouldWriteStagedFilesAndRecordNewPaths() throws IOException {
    Path created = dir.resolve("Created.java");
    stagingArea.stage(new StagedFile(created.toString(), null, "class Created {}", true, true));

    ApplyStagedSourceAction.Response response = action.handle(new JsonObject());

    assertEquals(1, response.getApplied().size());
    assertEquals("class Created {}", Files.readString(created, StandardCharsets.UTF_8));
    registryMock.verify(() -> SourcePathRegistry.record(created.toString()));
    assertTrue(stagingArea.list().isEmpty());
  }

  @Test
  @DisplayName("Should report a failure code when apply is refused")
  void shouldReportFailureWhenApplyRefused() throws IOException {
    Path existing = dir.resolve("Stale.java");
    Files.writeString(existing, "class Stale { int edited; }", StandardCharsets.UTF_8);
    stagingArea.stage(new StagedFile(existing.toString(), SourceHasher.hash("class Stale {}"),
        "class Stale { int x; }", false, true));

    ApplyStagedSourceAction.Response response = action.handle(new JsonObject());

    assertTrue(response.getApplied().isEmpty());
    assertEquals("SOURCE_CHANGED", response.getCode());
    assertTrue(response.isRestored());
  }

  @Test
  @DisplayName("Should return an empty result when nothing is staged")
  void shouldReturnEmptyResultWhenNothingStaged() {
    ApplyStagedSourceAction.Response response = action.handle(new JsonObject());

    assertTrue(response.getApplied().isEmpty());
  }
}
