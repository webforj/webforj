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
          config.setDescription(
              "Minifier implementations for webforJ minify plugin (e.g., ph-css, closure-compiler)");
        });

    // Get source sets
    SourceSetContainer sourceSets =
        project.getExtensions().getByType(SourceSetContainer.class);
    SourceSet mainSourceSet = sourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME);

    // Register minify task
    var minifyTask = project.getTasks().register("minify", MinifyTask.class,
        task -> configureMinifyTask(task, mainSourceSet, extension, project, minifierConfig));

    // Make jar/war task depend on minify (must be done outside register block)
    configureJarTaskDependency(project, minifyTask);

    // Also configure war task if present
    configureWarTaskDependency(project, minifyTask);

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

  private void configureMinifyTask(MinifyTask task, SourceSet mainSourceSet,
      MinifyExtension extension, Project project, Configuration minifierConfig) {
    // Configure task inputs
    task.getOutputDirectory().set(mainSourceSet.getOutput().getClassesDirs().getSingleFile());
    task.getResourcesDirectory()
        .set(mainSourceSet.getResources().getSourceDirectories().getSingleFile());
    task.getSkip().set(extension.getSkip());
    task.getMinifierClasspath().from(minifierConfig);

    // Run after classes task
    task.dependsOn(project.getTasks().named("classes"));
  }
}
