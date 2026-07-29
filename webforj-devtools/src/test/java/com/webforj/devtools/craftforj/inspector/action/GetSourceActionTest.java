package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceHasher;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

class GetSourceActionTest {

  private MockedStatic<SourcePathRegistry> registryMock;

  @BeforeEach
  void setUp() {
    registryMock = mockStatic(SourcePathRegistry.class);
    registryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);
  }

  @AfterEach
  void tearDown() {
    registryMock.close();
  }

  @Test
  @DisplayName("Should return file content successfully")
  void shouldReturnFileContentSuccessfully() {
    String expectedContent = "public class Test {}";
    GetSourceAction action = new GetSourceAction(path -> expectedContent);

    JsonObject params = new JsonObject();
    params.addProperty("file", "/path/to/Test.java");

    GetSourceAction.Response response = action.handle(params);

    assertEquals(expectedContent, response.getContent());
  }

  @Test
  @DisplayName("Should return the content hash alongside the content")
  void shouldReturnContentHash() {
    String expectedContent = "public class Test {}";
    GetSourceAction action = new GetSourceAction(path -> expectedContent);

    JsonObject params = new JsonObject();
    params.addProperty("file", "/path/to/Test.java");

    GetSourceAction.Response response = action.handle(params);

    assertEquals(SourceHasher.hash(expectedContent), response.getContentHash());
  }

  @Test
  @DisplayName("Should throw exception when file parameter is missing")
  void shouldThrowWhenFileMissing() {
    GetSourceAction action = new GetSourceAction(path -> null);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
    assertTrue(ex.getMessage().contains("Missing file"));
  }

  @Test
  @DisplayName("Should throw exception when file not found")
  void shouldThrowWhenFileNotFound() {
    GetSourceAction action = new GetSourceAction(path -> null);

    JsonObject params = new JsonObject();
    params.addProperty("file", "/non/existent/File.java");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("File not found"));
  }

  @Test
  @DisplayName("Should reject a file the server never resolved")
  void shouldRejectUnrecordedFile() {
    registryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(false);
    GetSourceAction action = new GetSourceAction(path -> "secret");

    JsonObject params = new JsonObject();
    params.addProperty("file", "/etc/passwd");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("not a recorded component source"));
  }

  @Test
  @DisplayName("Should fail with an action error for a non-UTF-8 source file")
  void shouldFailOnNonUtf8File(@TempDir Path dir) throws IOException {
    Path file = dir.resolve("Bad.java");
    Files.write(file, new byte[] {(byte) 0xC3, (byte) 0x28, (byte) 0xFF});
    GetSourceAction action = new GetSourceAction();

    JsonObject params = new JsonObject();
    params.addProperty("file", file.toString());

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Failed to read file"));
  }

  @Test
  @DisplayName("Should propagate exception when file read fails")
  void shouldPropagateExceptionWhenFileReadFails() {
    GetSourceAction action = new GetSourceAction(path -> {
      throw new RuntimeException("Read error");
    });

    JsonObject params = new JsonObject();
    params.addProperty("file", "/path/to/File.java");

    RuntimeException ex = assertThrows(RuntimeException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Read error"));
  }
}
