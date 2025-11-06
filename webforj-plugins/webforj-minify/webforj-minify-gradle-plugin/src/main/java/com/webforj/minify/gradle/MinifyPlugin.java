package com.webforj.minify.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.tasks.SourceSet;
import org.gradle.api.tasks.SourceSetContainer;

/**
 * Gradle plugin that registers the minify task for webforJ applications.
 *
 * <p>The plugin automatically configures the minify task to run after the classes task,
 * ensuring assets are minified before packaging.
 *
 * @author Kevin Hagel
 */
public class MinifyPlugin implements Plugin<Project> {

  @Override
  public void apply(Project project) {
    // Ensure Java plugin is applied
    project.getPlugins().apply(JavaPlugin.class);

    // Create extension for configuration
    MinifyExtension extension =
        project.getExtensions().create("webforjMinify", MinifyExtension.class);

    // Create minifier configuration for minifier implementations
    Configuration minifierConfig =
        project.getConfigurations().create("webforjMinifier", config -> {
          config.setVisible(false);
          config.setCanBeConsumed(false);
          config.setDescription("Minifier implementations for webforJ minify plugin "
              + "(e.g., ph-css, closure-compiler)");
        });

    // Get source sets
    SourceSetContainer sourceSets =
        project.getExtensions().getByType(SourceSetContainer.class);
    SourceSet mainSourceSet = sourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME);

    // Register minify task
    var minifyTask = project.getTasks().register("minify", MinifyTask.class,
        task -> configureMinifyTask(task, mainSourceSet, extension, project, minifierConfig));

    // Make jar/war/bootJar task depend on minify (must be done outside register block)
    configureJarTaskDependency(project, minifyTask);

    // Also configure war task if present
    configureWarTaskDependency(project, minifyTask);

    // Also configure bootJar task if present (Spring Boot)
    configureBootJarTaskDependency(project, minifyTask);

    project.getLogger().debug("webforJ Minify Plugin applied to project: {}", project.getName());
  }

  private void configureJarTaskDependency(Project project,
      org.gradle.api.tasks.TaskProvider<MinifyTask> minifyTask) {
    project.getTasks().named("jar").configure(jarTask -> jarTask.dependsOn(minifyTask));
  }

  private void configureWarTaskDependency(Project project,
      org.gradle.api.tasks.TaskProvider<MinifyTask> minifyTask) {
    project.getPlugins().withId("war", plugin -> {
      project.getTasks().named("war").configure(warTask -> warTask.dependsOn(minifyTask));
    });
  }

  private void configureBootJarTaskDependency(Project project,
      org.gradle.api.tasks.TaskProvider<MinifyTask> minifyTask) {
    project.getPlugins().withId("org.springframework.boot", plugin -> {
      project.getTasks().named("bootJar").configure(bootJarTask -> bootJarTask.dependsOn(minifyTask));
    });
  }

  private void configureMinifyTask(MinifyTask task, SourceSet mainSourceSet,
      MinifyExtension extension, Project project, Configuration minifierConfig) {
    // Set task metadata
    task.setGroup("webforJ");
    task.setDescription("Minifies webforJ assets");

    // Configure task inputs using Provider API for lazy configuration
    // Use layout.dir() with provider to safely handle file collections
    task.getOutputDirectory().set(
        project.getLayout().dir(
            project.provider(() -> mainSourceSet.getOutput().getClassesDirs().getFiles()
                .stream()
                .findFirst()
                .orElseThrow(() -> new org.gradle.api.GradleException(
                    "No classes directory found for source set: " + mainSourceSet.getName())))));

    // FIXED: Use output resources directory (build/resources/main), not source directory
    task.getResourcesDirectory().set(mainSourceSet.getOutput().getResourcesDir());

    task.getSkip().set(extension.getSkip());
    task.getMinifierClasspath().from(minifierConfig);

    // Run after classes task (which includes processResources)
    task.dependsOn(project.getTasks().named("classes"));
  }
}
