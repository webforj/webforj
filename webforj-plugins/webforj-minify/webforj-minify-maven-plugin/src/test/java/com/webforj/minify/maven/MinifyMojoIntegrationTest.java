package com.webforj.minify.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Integration tests for MinifyMojo that test actual plugin execution.
 *
 * <p>
 * These tests create real file structures and execute the mojo to verify minification works
 * end-to-end.
 *
 * @author Kevin Hagel
 */
class MinifyMojoIntegrationTest {

  private MinifyMojo mojo;
  private MavenProject project;

  @BeforeEach
  void setUp() {
    mojo = new MinifyMojo();
    mojo.setLog(new SystemStreamLog());
    project = new MavenProject();
  }

  @Test
  void testMinifyJavaScriptFile(@TempDir Path tempDir) throws Exception {
    // Setup test project structure
    Path classesDir = tempDir.resolve("target/classes");
    Path jsDir = classesDir.resolve("static/js");
    Files.createDirectories(jsDir);

    // Create test JavaScript file
    Path jsFile = jsDir.resolve("test.js");
    String originalJs = """
        function greet(name) {
          const message = "Hello, " + name + "!";
          return message;
        }

        function calculateSum(a, b) {
          const result = a + b;
          return result;
        }
        """;
    Files.writeString(jsFile, originalJs);

    long originalSize = Files.size(jsFile);

    // Create webforj-minify.txt config file to tell plugin what to process
    Path srcDir = tempDir.resolve("src/main/resources/META-INF");
    Files.createDirectories(srcDir);
    Path configFile = srcDir.resolve("webforj-minify.txt");
    Files.writeString(configFile, "static/js/*.js\n");

    // Configure and execute mojo
    setField(mojo, "project", project);
    setField(mojo, "outputDirectory", classesDir.toString());
    setField(mojo, "skip", false);

    // Add Closure JS configuration
    Map<String, Map<String, String>> config = new HashMap<>();
    Map<String, String> closureConfig = new HashMap<>();
    closureConfig.put("compilationLevel", "SIMPLE_OPTIMIZATIONS");
    closureConfig.put("languageIn", "ECMASCRIPT_2020");
    closureConfig.put("languageOut", "ECMASCRIPT5");
    config.put("closureJs", closureConfig);
    setField(mojo, "minifierConfigurations", config);

    // Set base directory for config file resolution
    File baseDir = tempDir.toFile();
    project.setFile(new File(baseDir, "pom.xml"));

    // Execute minification
    mojo.execute();

    // Verify file was minified
    assertTrue(Files.exists(jsFile), "JavaScript file should still exist");

    long minifiedSize = Files.size(jsFile);
    String minifiedContent = Files.readString(jsFile);

    // Minified file should be smaller
    assertTrue(minifiedSize < originalSize, String.format(
        "File should be minified (original: %d, minified: %d)", originalSize, minifiedSize));

    // Minified content should not be empty
    assertFalse(minifiedContent.trim().isEmpty(), "Minified content should not be empty");

    // Should contain function names (SIMPLE_OPTIMIZATIONS preserves top-level names)
    assertTrue(minifiedContent.contains("greet") || minifiedContent.contains("function"),
        "Minified content should contain code");
  }

  @Test
  void testMinifyCssFile(@TempDir Path tempDir) throws Exception {
    // Setup test project structure
    Path classesDir = tempDir.resolve("target/classes");
    Path cssDir = classesDir.resolve("static/css");
    Files.createDirectories(cssDir);

    // Create test CSS file
    Path cssFile = cssDir.resolve("test.css");
    String originalCss = """
        body {
          background-color: white;
          color: black;
          font-family: Arial, sans-serif;
        }

        .container {
          max-width: 1200px;
          margin: 0 auto;
          padding: 20px;
        }
        """;
    Files.writeString(cssFile, originalCss);

    long originalSize = Files.size(cssFile);

    // Create webforj-minify.txt config file
    Path srcDir = tempDir.resolve("src/main/resources/META-INF");
    Files.createDirectories(srcDir);
    Path configFile = srcDir.resolve("webforj-minify.txt");
    Files.writeString(configFile, "static/css/*.css\n");

    // Configure and execute mojo
    setField(mojo, "project", project);
    setField(mojo, "outputDirectory", classesDir.toString());
    setField(mojo, "skip", false);

    File baseDir = tempDir.toFile();
    project.setFile(new File(baseDir, "pom.xml"));

    // Execute minification
    mojo.execute();

    // Verify file was minified
    assertTrue(Files.exists(cssFile), "CSS file should still exist");

    long minifiedSize = Files.size(cssFile);
    String minifiedContent = Files.readString(cssFile);

    // Minified file should be smaller or same size
    assertTrue(minifiedSize <= originalSize, String
        .format("CSS should be minified (original: %d, minified: %d)", originalSize, minifiedSize));

    // Minified content should not be empty
    assertFalse(minifiedContent.trim().isEmpty(), "Minified CSS should not be empty");

    // Should contain CSS selectors
    assertTrue(minifiedContent.contains("body") || minifiedContent.contains(".container"),
        "Minified content should contain CSS");
  }

  @Test
  void testSkipMinification(@TempDir Path tempDir) throws Exception {
    // Setup test project structure
    Path classesDir = tempDir.resolve("target/classes");
    Path jsDir = classesDir.resolve("static/js");
    Files.createDirectories(jsDir);

    // Create test file
    Path jsFile = jsDir.resolve("test.js");
    String originalContent = "function test() { return 42; }";
    Files.writeString(jsFile, originalContent);

    // Configure mojo with skip=true
    setField(mojo, "project", project);
    setField(mojo, "outputDirectory", classesDir.toString());
    setField(mojo, "skip", true);

    File baseDir = tempDir.toFile();
    project.setFile(new File(baseDir, "pom.xml"));

    // Execute mojo
    mojo.execute();

    // Verify file was NOT modified
    String contentAfter = Files.readString(jsFile);
    assertEquals(originalContent, contentAfter, "File should not be modified when skip=true");
  }

  @Test
  void testSkipAlreadyMinifiedFiles(@TempDir Path tempDir) throws Exception {
    // Setup test project structure
    Path classesDir = tempDir.resolve("target/classes");
    Path jsDir = classesDir.resolve("static/js");
    Files.createDirectories(jsDir);

    // Create already minified file
    Path minFile = jsDir.resolve("test.min.js");
    String originalContent = "function test(){return 42}";
    Files.writeString(minFile, originalContent);

    // Configure and execute mojo
    setField(mojo, "project", project);
    setField(mojo, "outputDirectory", classesDir.toString());
    setField(mojo, "skip", false);

    File baseDir = tempDir.toFile();
    project.setFile(new File(baseDir, "pom.xml"));

    // Execute minification
    mojo.execute();

    // Verify .min.js file was NOT processed
    String contentAfter = Files.readString(minFile);
    assertEquals(originalContent, contentAfter,
        "Already minified files (.min.js) should be skipped");
  }

  @Test
  void testProcessMultipleFiles(@TempDir Path tempDir) throws Exception {
    // Setup test project structure
    Path classesDir = tempDir.resolve("target/classes");
    Path staticDir = classesDir.resolve("static");
    Files.createDirectories(staticDir.resolve("js"));
    Files.createDirectories(staticDir.resolve("css"));

    // Create multiple test files
    Files.writeString(staticDir.resolve("js/app.js"), "function app() { return 1; }");
    Files.writeString(staticDir.resolve("js/util.js"), "function util() { return 2; }");
    Files.writeString(staticDir.resolve("css/main.css"), "body { margin: 0; }");
    Files.writeString(staticDir.resolve("css/theme.css"), ".theme { color: red; }");

    // Create webforj-minify.txt config file
    Path srcDir = tempDir.resolve("src/main/resources/META-INF");
    Files.createDirectories(srcDir);
    Path configFile = srcDir.resolve("webforj-minify.txt");
    Files.writeString(configFile, "static/js/*.js\nstatic/css/*.css\n");

    // Configure and execute mojo
    setField(mojo, "project", project);
    setField(mojo, "outputDirectory", classesDir.toString());
    setField(mojo, "skip", false);

    File baseDir = tempDir.toFile();
    project.setFile(new File(baseDir, "pom.xml"));

    // Execute minification
    mojo.execute();

    // Verify all files exist and were processed
    assertTrue(Files.exists(staticDir.resolve("js/app.js")));
    assertTrue(Files.exists(staticDir.resolve("js/util.js")));
    assertTrue(Files.exists(staticDir.resolve("css/main.css")));
    assertTrue(Files.exists(staticDir.resolve("css/theme.css")));

    // Verify files have content
    assertTrue(Files.size(staticDir.resolve("js/app.js")) > 0);
    assertTrue(Files.size(staticDir.resolve("css/main.css")) > 0);
  }

  /**
   * Helper method to set private field value using reflection.
   */
  private void setField(Object target, String fieldName, Object value) throws Exception {
    java.lang.reflect.Field field = target.getClass().getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(target, value);
  }
}
