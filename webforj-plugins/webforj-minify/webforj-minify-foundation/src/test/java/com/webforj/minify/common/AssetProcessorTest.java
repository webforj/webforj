package com.webforj.minify.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Unit tests for AssetProcessor.
 *
 * @author Kevin Hagel
 */
class AssetProcessorTest {

  @TempDir
  Path tempDir;

  private TestBuildLogger logger;
  private AssetProcessor processor;

  @BeforeEach
  void setUp() {
    logger = new TestBuildLogger();
    processor = new AssetProcessor(logger);
  }

  @Test
  void testGetRegistry() {
    MinifierRegistry registry = processor.getRegistry();
    assertNotNull(registry);
  }

  @Test
  void testGetProcessedFileCount() {
    assertEquals(0, processor.getProcessedFileCount());
  }

  @Test
  void testProcessManifestWithValidJson() throws IOException {
    // Create test CSS file
    Path staticDir = tempDir.resolve("static");
    Files.createDirectories(staticDir);
    Path cssFile = staticDir.resolve("test.css");
    Files.writeString(cssFile, "body { color: red; }");

    // Create manifest
    Path manifestPath = tempDir.resolve("manifest.json");
    Files.writeString(manifestPath, "{"
        + "\"assets\": ["
        + "{\"url\": \"ws://test.css\", \"type\": \"StyleSheet\"}"
        + "]"
        + "}");

    // Register test minifier
    processor.getRegistry().register(new TestCssMinifier());

    // Process manifest
    ResourceResolver resolver = new ResourceResolver(tempDir);
    java.util.Set<java.nio.file.Path> files = processor.collectManifestFiles(manifestPath, resolver);
    processor.processFiles(files);

    // Verify file was processed
    assertEquals(1, processor.getProcessedFileCount());
    String minified = Files.readString(cssFile);
    assertEquals("MINIFIED", minified);
  }

  @Test
  void testProcessManifestWithLegacyResourcesFormat() throws IOException {
    // Create test CSS file
    Path staticDir = tempDir.resolve("static");
    Files.createDirectories(staticDir);
    Path cssFile = staticDir.resolve("legacy.css");
    Files.writeString(cssFile, "body { color: blue; }");

    // Create manifest with legacy "resources" key
    Path manifestPath = tempDir.resolve("manifest.json");
    Files.writeString(manifestPath, "{"
        + "\"resources\": ["
        + "{\"url\": \"ws://legacy.css\", \"type\": \"StyleSheet\"}"
        + "]"
        + "}");

    // Register test minifier
    processor.getRegistry().register(new TestCssMinifier());

    // Process manifest
    ResourceResolver resolver = new ResourceResolver(tempDir);
    java.util.Set<java.nio.file.Path> files = processor.collectManifestFiles(manifestPath, resolver);
    processor.processFiles(files);

    // Verify file was processed
    assertEquals(1, processor.getProcessedFileCount());
  }

  @Test
  void testProcessManifestWithEmptyAssets() throws IOException {
    Path manifestPath = tempDir.resolve("manifest.json");
    Files.writeString(manifestPath, "{\"assets\": []}");

    ResourceResolver resolver = new ResourceResolver(tempDir);
    java.util.Set<java.nio.file.Path> files = processor.collectManifestFiles(manifestPath, resolver);
    processor.processFiles(files);

    assertEquals(0, processor.getProcessedFileCount());
    assertTrue(logger.warnings.stream()
        .anyMatch(msg -> msg.contains("no assets")));
  }

  @Test
  void testProcessManifestWithMalformedJson() throws IOException {
    Path manifestPath = tempDir.resolve("manifest.json");
    Files.writeString(manifestPath, "{invalid json");

    ResourceResolver resolver = new ResourceResolver(tempDir);

    assertThrows(com.google.gson.JsonSyntaxException.class, () -> {
      java.util.Set<java.nio.file.Path> files = processor.collectManifestFiles(manifestPath, resolver);
    processor.processFiles(files);
    });
  }

  @Test
  void testProcessConfigFile() throws IOException {
    // Create test directory structure
    Path resourcesRoot = tempDir.resolve("resources");
    Path cssDir = resourcesRoot.resolve("css");
    Files.createDirectories(cssDir);

    // Create test CSS files
    Path css1 = cssDir.resolve("style1.css");
    Path css2 = cssDir.resolve("style2.css");
    Files.writeString(css1, "/* css1 */");
    Files.writeString(css2, "/* css2 */");

    // Create config file
    Path configPath = tempDir.resolve("config.txt");
    Files.writeString(configPath, "css/*.css\n");

    // Register test minifier
    processor.getRegistry().register(new TestCssMinifier());

    // Process config file
    java.util.Set<java.nio.file.Path> files = processor.collectConfigFiles(configPath, resourcesRoot);
    processor.processFiles(files);

    // Verify files were processed
    assertEquals(2, processor.getProcessedFileCount());
  }

  @Test
  void testProcessConfigFileWithExclusions() throws IOException {
    // Create test directory structure
    Path resourcesRoot = tempDir.resolve("resources");
    Path cssDir = resourcesRoot.resolve("css");
    Files.createDirectories(cssDir);

    // Create test CSS files
    Path css1 = cssDir.resolve("style1.css");
    Path css2 = cssDir.resolve("vendor.css");
    Files.writeString(css1, "/* css1 */");
    Files.writeString(css2, "/* vendor */");

    // Create config file with exclusion
    Path configPath = tempDir.resolve("config.txt");
    Files.writeString(configPath, "css/*.css\n!css/vendor.css\n");

    // Register test minifier
    processor.getRegistry().register(new TestCssMinifier());

    // Process config file
    java.util.Set<java.nio.file.Path> files = processor.collectConfigFiles(configPath, resourcesRoot);
    processor.processFiles(files);

    // Verify only one file was processed (vendor.css excluded)
    assertEquals(1, processor.getProcessedFileCount());
  }

  @Test
  void testProcessConfigFileWithMultiLevelExclusion() throws IOException {
    // Test for bug fix: !**/*.js should exclude ALL JavaScript files at any depth
    Path resourcesRoot = tempDir.resolve("resources");
    Path jsDir = resourcesRoot.resolve("js");
    Path libDir = jsDir.resolve("lib");
    Path vendorDir = libDir.resolve("vendor");
    Files.createDirectories(vendorDir);

    // Create JS files at multiple levels
    Path js1 = jsDir.resolve("app.js");
    Path js2 = libDir.resolve("utils.js");
    Path js3 = vendorDir.resolve("jquery.js");
    Files.writeString(js1, "console.log('app');");
    Files.writeString(js2, "console.log('utils');");
    Files.writeString(js3, "console.log('jquery');");

    // Create config file with multi-level exclusion pattern
    Path configPath = tempDir.resolve("config.txt");
    Files.writeString(configPath, "js/**/*.js\n!**/*.js\n");

    // Register test minifier for JS
    processor.getRegistry().register(new TestJsMinifier());

    // Process config file
    java.util.Set<java.nio.file.Path> files = processor.collectConfigFiles(configPath, resourcesRoot);
    processor.processFiles(files);

    // Verify NO files were processed (all excluded by !**/*.js)
    assertEquals(0, processor.getProcessedFileCount());
  }

  @Test
  void testProcessConfigFileWithComments() throws IOException {
    // Create test directory structure
    Path resourcesRoot = tempDir.resolve("resources");
    Path cssDir = resourcesRoot.resolve("css");
    Files.createDirectories(cssDir);
    Path css1 = cssDir.resolve("style.css");
    Files.writeString(css1, "/* css */");

    // Create config file with comments and empty lines
    Path configPath = tempDir.resolve("config.txt");
    Files.writeString(configPath, "# This is a comment\n\ncss/*.css\n\n# Another comment\n");

    // Register test minifier
    processor.getRegistry().register(new TestCssMinifier());

    // Process config file
    java.util.Set<java.nio.file.Path> files = processor.collectConfigFiles(configPath, resourcesRoot);
    processor.processFiles(files);

    // Verify file was processed
    assertEquals(1, processor.getProcessedFileCount());
  }

  /**
   * Test implementation of BuildLogger.
   */
  private static class TestBuildLogger implements BuildLogger {
    List<String> infos = new ArrayList<>();
    List<String> warnings = new ArrayList<>();
    List<String> debugs = new ArrayList<>();
    List<String> errors = new ArrayList<>();

    @Override
    public void info(String message) {
      infos.add(message);
    }

    @Override
    public void warn(String message) {
      warnings.add(message);
    }

    @Override
    public void debug(String message) {
      debugs.add(message);
    }

    @Override
    public void error(String message, Throwable throwable) {
      errors.add(message + ": " + throwable.getMessage());
    }
  }

  /**
   * Test minifier for CSS files.
   */
  private static class TestCssMinifier implements AssetMinifier {
    @Override
    public String minify(String content, Path sourceFile) {
      return "MINIFIED";
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of("css");
    }
  }

  /**
   * Test minifier for JS files.
   */
  private static class TestJsMinifier implements AssetMinifier {
    @Override
    public String minify(String content, Path sourceFile) {
      return "MINIFIED_JS";
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of("js");
    }
  }
}
