package com.webforj.minify.gradle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.util.Map;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Unit tests for MinifyPlugin.
 *
 * <p>
 * Tests verify that the plugin can be applied and registers its extension correctly.
 * </p>
 *
 * @author Kevin Hagel
 */
class MinifyPluginTest {

  @TempDir
  File testProjectDir;
  private Project project;

  @BeforeEach
  void setup() {
    // Create a test project with proper directory structure
    project = ProjectBuilder.builder().withProjectDir(testProjectDir).build();

    // Create minimal source directories for Java plugin
    new File(testProjectDir, "src/main/java").mkdirs();
    new File(testProjectDir, "src/main/resources").mkdirs();
    new File(testProjectDir, "build/classes/java/main").mkdirs();
  }

  @Test
  void pluginCanBeApplied() {
    // Apply the plugin - this should not throw
    project.getPlugins().apply("com.webforj.minify");

    // Verify plugin is applied
    assertTrue(project.getPlugins().hasPlugin("com.webforj.minify"), "Plugin should be applied");
  }

  @Test
  void pluginRegistersExtension() {
    // Apply the plugin
    project.getPlugins().apply("com.webforj.minify");

    // Verify the extension exists
    assertNotNull(project.getExtensions().findByName("webforjMinify"),
        "webforjMinify extension should be registered");
  }

  @Test
  void extensionHasSkipProperty() {
    // Apply the plugin
    project.getPlugins().apply("com.webforj.minify");

    // Get the extension
    MinifyExtension extension = project.getExtensions().findByType(MinifyExtension.class);
    assertNotNull(extension, "Extension should be available");
    assertNotNull(extension.getSkip(), "Extension should have skip property");
  }

  @Test
  void javaPluginIsAutomaticallyApplied() {
    // Apply the minify plugin
    project.getPlugins().apply("com.webforj.minify");

    // Verify Java plugin was also applied
    assertTrue(project.getPlugins().hasPlugin("java"),
        "Java plugin should be automatically applied");
  }

  @Test
  void pluginRegistersMinifyTaskProvider() {
    // Apply the plugin
    project.getPlugins().apply("com.webforj.minify");

    // Verify the task provider exists (but don't realize it)
    assertTrue(project.getTasks().getNames().contains("minify"),
        "minify task should be registered");
  }

  @Test
  void extensionHasMinifierConfigurationsProperty() {
    project.getPlugins().apply("com.webforj.minify");

    MinifyExtension extension = project.getExtensions().findByType(MinifyExtension.class);
    assertNotNull(extension, "Extension should be available");
    assertNotNull(extension.getMinifierConfigurations(),
        "Extension should have minifierConfigurations property");
  }

  @Test
  void minifierConfigurationsDefaultsToEmpty() {
    project.getPlugins().apply("com.webforj.minify");

    MinifyExtension extension = project.getExtensions().findByType(MinifyExtension.class);
    assertNotNull(extension);
    assertTrue(extension.getMinifierConfigurations().getOrElse(Map.of()).isEmpty(),
        "Default minifier configurations should be empty");
  }

  @Test
  void minifierConfigurationsAcceptsValues() {
    project.getPlugins().apply("com.webforj.minify");

    MinifyExtension extension = project.getExtensions().findByType(MinifyExtension.class);
    assertNotNull(extension);

    Map<String, String> closureOptions = Map.of("compilationLevel", "SIMPLE_OPTIMIZATIONS",
        "languageIn", "ECMASCRIPT_NEXT", "languageOut", "ECMASCRIPT5");
    extension.getMinifierConfigurations().put("closureJs", closureOptions);

    Map<String, Map<String, String>> result = extension.getMinifierConfigurations().get();
    assertEquals(1, result.size(), "Should have one configuration entry");
    assertTrue(result.containsKey("closureJs"), "Should contain closureJs key");
    assertEquals("SIMPLE_OPTIMIZATIONS", result.get("closureJs").get("compilationLevel"));
  }

  @Test
  void taskReceivesMinifierConfigurationsFromExtension() {
    project.getPlugins().apply("com.webforj.minify");

    MinifyExtension extension = project.getExtensions().findByType(MinifyExtension.class);
    assertNotNull(extension);

    Map<String, String> options = Map.of("compilationLevel", "ADVANCED_OPTIMIZATIONS");
    extension.getMinifierConfigurations().put("closureJs", options);

    // Realize the task and verify config was wired
    MinifyTask task = (MinifyTask) project.getTasks().getByName("minify");
    Map<String, Map<String, String>> taskConfig = task.getMinifierConfigurations().get();
    assertTrue(taskConfig.containsKey("closureJs"),
        "Task should receive closureJs config from extension");
    assertEquals("ADVANCED_OPTIMIZATIONS", taskConfig.get("closureJs").get("compilationLevel"));
  }
}
