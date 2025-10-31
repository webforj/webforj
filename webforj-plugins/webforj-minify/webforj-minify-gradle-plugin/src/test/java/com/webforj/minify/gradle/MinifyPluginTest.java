package com.webforj.minify.gradle;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Unit tests for MinifyPlugin.
 *
 * <p>Tests verify that the plugin can be applied and registers its extension correctly.
 *
 * @author Kevin Hagel
 */
class MinifyPluginTest {

  @TempDir File testProjectDir;
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
    assertTrue(project.getPlugins().hasPlugin("com.webforj.minify"),
        "Plugin should be applied");
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
}
