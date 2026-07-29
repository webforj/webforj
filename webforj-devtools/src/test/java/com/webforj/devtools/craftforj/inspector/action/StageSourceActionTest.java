package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.CompileValidator;
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

class StageSourceActionTest {

  @TempDir
  Path projectRoot;

  private MockedStatic<SourcePathRegistry> registryMock;
  private final Map<String, StagedFile> storage = new LinkedHashMap<>();
  private SourceStagingArea stagingArea;
  private StageSourceAction action;
  private Path sourceRoot;

  @BeforeEach
  void setUp() throws IOException {
    registryMock = mockStatic(SourcePathRegistry.class);
    registryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);
    stagingArea = new SourceStagingArea(() -> storage);
    action = new StageSourceAction(stagingArea, new CompileValidator(), projectRoot);
    sourceRoot = projectRoot.resolve("src").resolve("main").resolve("java");
    Files.createDirectories(sourceRoot);
  }

  @AfterEach
  void tearDown() {
    registryMock.close();
  }

  @Test
  @DisplayName("Should stage a compiling new file under a source root")
  void shouldStageCompilingNewFile() {
    JsonObject params = params(sourceRoot.resolve("Fresh.java"), """
        public class Fresh {
        }
        """);

    StageSourceAction.Response response = action.handle(params);

    assertTrue(response.isStaged());
    assertTrue(response.isVerified());
    assertTrue(response.isNewFile());
    assertEquals(1, stagingArea.list().size());
  }

  @Test
  @DisplayName("Should refuse a new file outside every source root")
  void shouldRefuseNewFileOutsideSourceRoots() {
    JsonObject params = params(projectRoot.resolve("Outside.java"), """
        public class Outside {
        }
        """);

    StageSourceAction.Response response = action.handle(params);

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_PATH_REFUSED, response.getCode());
    assertTrue(stagingArea.list().isEmpty());
  }

  @Test
  @DisplayName("Should refuse Kotlin sources")
  void shouldRefuseKotlinSources() {
    JsonObject params = params(sourceRoot.resolve("View.kt"), "class View");

    StageSourceAction.Response response = action.handle(params);

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_PATH_REFUSED, response.getCode());
    assertTrue(response.getMessage().contains("Kotlin"));
  }

  @Test
  @DisplayName("Should refuse overwriting a file the server never resolved")
  void shouldRefuseUnrecordedExistingFile() throws IOException {
    Path existing = sourceRoot.resolve("Secret.java");
    Files.writeString(existing, "class Secret {}", StandardCharsets.UTF_8);
    registryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(false);

    JsonObject params = params(existing, "class Secret { int x; }");
    params.addProperty("baseHash", SourceHasher.hash("class Secret {}"));

    StageSourceAction.Response response = action.handle(params);

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_PATH_REFUSED, response.getCode());
  }

  @Test
  @DisplayName("Should reject an edit whose base hash no longer matches the disk")
  void shouldRejectStaleBaseHash() throws IOException {
    Path existing = sourceRoot.resolve("Stale.java");
    Files.writeString(existing, "class Stale { int edited; }", StandardCharsets.UTF_8);

    JsonObject params = params(existing, "class Stale { int x; }");
    params.addProperty("baseHash", SourceHasher.hash("class Stale {}"));

    StageSourceAction.Response response = action.handle(params);

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_SOURCE_CHANGED, response.getCode());
  }

  @Test
  @DisplayName("Should reject an edit without a base hash")
  void shouldRejectEditWithoutBaseHash() throws IOException {
    Path existing = sourceRoot.resolve("NoHash.java");
    Files.writeString(existing, "class NoHash {}", StandardCharsets.UTF_8);

    StageSourceAction.Response response = action.handle(params(existing, "class NoHash {}"));

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_SOURCE_CHANGED, response.getCode());
  }

  @Test
  @DisplayName("Should return diagnostics for a non compiling candidate")
  void shouldReturnDiagnosticsForNonCompilingCandidate() {
    JsonObject params = params(sourceRoot.resolve("Broken.java"), """
        public class Broken {
          UnknownType field;
        }
        """);

    StageSourceAction.Response response = action.handle(params);

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_COMPILE_ERROR, response.getCode());
    assertFalse(response.getErrors().isEmpty());
    assertTrue(stagingArea.list().isEmpty());
  }

  @Test
  @DisplayName("Should stage an edit of a recorded file with a matching base hash")
  void shouldStageEditWithMatchingBaseHash() throws IOException {
    Path existing = sourceRoot.resolve("Edited.java");
    String original = """
        public class Edited {
        }
        """;
    Files.writeString(existing, original, StandardCharsets.UTF_8);

    JsonObject params = params(existing, """
        public class Edited {
          private int count;
        }
        """);
    params.addProperty("baseHash", SourceHasher.hash(original));

    StageSourceAction.Response response = action.handle(params);

    assertTrue(response.isStaged());
    assertTrue(response.isVerified());
    assertFalse(response.isNewFile());
  }

  @Test
  @DisplayName("Should refuse a request without path or content")
  void shouldRefuseMissingPathOrContent() {
    StageSourceAction.Response response = action.handle(new JsonObject());

    assertFalse(response.isStaged());
    assertEquals(StageSourceAction.CODE_PATH_REFUSED, response.getCode());
  }

  private static JsonObject params(Path path, String content) {
    JsonObject params = new JsonObject();
    params.addProperty("path", path.toString());
    params.addProperty("content", content);

    return params;
  }
}
