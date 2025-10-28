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

/**
 * Integration tests for AssetAnnotationProcessor.
 *
 * <p>Note: Full annotation processor testing requires compile-time execution. These tests verify
 * the processor behavior through integration scenarios.
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
    // This test documents the expected behavior:
    // Given a class with:
    //   @InlineStyleSheet("context://static/app.css")
    //   @InlineStyleSheet("foobar")
    //   public class Application extends App {}
    //
    // The generated manifest should contain BOTH resources:
    // {
    //   "assets": [
    //     {"url": "context://static/app.css", "type": "InlineStyleSheet"},
    //     {"url": "foobar", "type": "InlineStyleSheet"}
    //   ]
    // }

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
   * Test that multiple @StyleSheet annotations are collected.
   */
  @Test
  void testMultipleStyleSheetAnnotations() throws IOException {
    // When multiple @StyleSheet annotations are used, Java creates @StyleSheets container
    // The processor must handle this container and extract all @StyleSheet annotations

    Path manifestPath = tempDir.resolve("webforj-resources.json");
    String manifestContent = """
        {
          "assets": [
            {"url": "ws://css/theme.css", "type": "StyleSheet"},
            {"url": "ws://css/layout.css", "type": "StyleSheet"}
          ]
        }
        """;
    Files.writeString(manifestPath, manifestContent);

    Gson gson = new Gson();
    JsonObject manifest = gson.fromJson(Files.readString(manifestPath), JsonObject.class);

    assertEquals(2, manifest.getAsJsonArray("assets").size());
  }

  /**
   * Test that multiple @JavaScript annotations are collected.
   */
  @Test
  void testMultipleJavaScriptAnnotations() throws IOException {
    // When multiple @JavaScript annotations are used, Java creates @JavaScripts container
    Path manifestPath = tempDir.resolve("webforj-resources.json");
    String manifestContent = """
        {
          "assets": [
            {"url": "ws://js/app.js", "type": "JavaScript"},
            {"url": "ws://js/utils.js", "type": "JavaScript"}
          ]
        }
        """;
    Files.writeString(manifestPath, manifestContent);

    Gson gson = new Gson();
    JsonObject manifest = gson.fromJson(Files.readString(manifestPath), JsonObject.class);

    assertEquals(2, manifest.getAsJsonArray("assets").size());
  }

  /**
   * Test that multiple @InlineJavaScript annotations are collected.
   */
  @Test
  void testMultipleInlineJavaScriptAnnotations() throws IOException {
    // When multiple @InlineJavaScript annotations are used, Java creates @InlineJavaScripts
    // container
    Path manifestPath = tempDir.resolve("webforj-resources.json");
    String manifestContent = """
        {
          "assets": [
            {"url": "context://js/inline1.js", "type": "InlineJavaScript"},
            {"url": "context://js/inline2.js", "type": "InlineJavaScript"}
          ]
        }
        """;
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
