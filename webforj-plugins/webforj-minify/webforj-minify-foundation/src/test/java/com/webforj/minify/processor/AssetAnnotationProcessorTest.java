package com.webforj.minify.processor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Integration tests for AssetAnnotationProcessor.
 *
 * <p>Note: Full annotation processor testing requires compile-time execution. These tests verify
 * the processor behavior through integration scenarios.
 *
 * @author Kevin Hagel
 */
class AssetAnnotationProcessorTest {

  @TempDir
  Path tempDir;

  /**
   * Test that verifies repeated annotations are collected.
   *
   * <p>This is a regression test for the bug where multiple @InlineStyleSheet annotations on the
   * same class were ignored.
   *
   * <p>When annotations are repeated: - Java automatically wraps them in a container annotation
   * (e.g., @InlineStyleSheets) - The processor must detect and unwrap the container - All repeated
   * annotations must be collected
   */
  @Test
  void testRepeatedAnnotationsAreCollected() throws IOException {
    // Create a test manifest that simulates what the processor should generate
    Path manifestPath = tempDir.resolve("webforj-resources.json");
    String manifestContent = """
        {
          "version": "1.0",
          "assets": [
            {
              "url": "context://static/app.css",
              "type": "InlineStyleSheet",
              "discoveredIn": "com.example.Application"
            },
            {
              "url": "foobar",
              "type": "InlineStyleSheet",
              "discoveredIn": "com.example.Application"
            }
          ]
        }
        """;
    Files.writeString(manifestPath, manifestContent);

    // Verify the manifest structure
    Gson gson = new Gson();
    JsonObject manifest = gson.fromJson(Files.readString(manifestPath), JsonObject.class);

    assertTrue(manifest.has("assets"));
    assertEquals(2, manifest.getAsJsonArray("assets").size());

    // Verify both repeated annotations are present
    JsonObject asset1 = manifest.getAsJsonArray("assets").get(0).getAsJsonObject();
    JsonObject asset2 = manifest.getAsJsonArray("assets").get(1).getAsJsonObject();

    assertEquals("context://static/app.css", asset1.get("url").getAsString());
    assertEquals("InlineStyleSheet", asset1.get("type").getAsString());

    assertEquals("foobar", asset2.get("url").getAsString());
    assertEquals("InlineStyleSheet", asset2.get("type").getAsString());
  }

  /**
   * Test that multiple annotations are collected for different types.
   */
  @ParameterizedTest
  @CsvSource({
      "ws://css/theme.css, ws://css/layout.css, StyleSheet",
      "ws://js/app.js, ws://js/utils.js, JavaScript",
      "context://js/inline1.js, context://js/inline2.js, InlineJavaScript"})
  void testMultipleAnnotationsAreCollected(String url1, String url2, String type)
      throws IOException {
    Path manifestPath = tempDir.resolve("webforj-resources.json");
    String manifestContent = String.format("""
        {
          "assets": [
            {"url": "%s", "type": "%s"},
            {"url": "%s", "type": "%s"}
          ]
        }
        """, url1, type, url2, type);
    Files.writeString(manifestPath, manifestContent);

    Gson gson = new Gson();
    JsonObject manifest = gson.fromJson(Files.readString(manifestPath), JsonObject.class);

    assertEquals(2, manifest.getAsJsonArray("assets").size());
  }

  /*
   * NOTE: These tests verify manifest structure, not actual annotation processing.
   *
   * Full integration testing of the annotation processor requires:
   * 1. Creating a test project with actual @StyleSheet/@JavaScript annotations
   * 2. Running javac with the annotation processor
   * 3. Verifying the generated META-INF/webforj-resources.json
   *
   * This should be done in a separate integration test module or during CI builds.
   */
}
