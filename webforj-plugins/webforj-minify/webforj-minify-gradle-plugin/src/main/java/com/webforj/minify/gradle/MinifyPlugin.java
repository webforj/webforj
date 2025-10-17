package com.webforj.minify.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.tasks.TaskProvider;

/**
 * Gradle plugin that adds asset minification to webforJ projects.
 *
 * The plugin automatically creates a 'minifyAssets' task that runs after
 * processResources and before jar/war tasks.
 */
public class MinifyPlugin implements Plugin<Project> {

  @Override
  public void apply(Project project) {
    // Create extension for configuration
    MinifyExtension extension = project.getExtensions().create("webforjMinify", MinifyExtension.class);

    // Register the minify task
    TaskProvider<MinifyTask> minifyTask = project.getTasks().register("minifyAssets", MinifyTask.class, task -> {
      task.setGroup("webforj");
      task.setDescription("Minifies webforJ CSS and JavaScript assets");
      task.getEnabled().set(extension.getEnabled());
    });

    // Configure task dependencies
    project.getPlugins().withType(JavaPlugin.class, javaPlugin -> {
      // Run after processResources
      Task processResources = project.getTasks().findByName("processResources");
      if (processResources != null) {
        minifyTask.configure(task -> task.dependsOn(processResources));
      }

      // Run before classes task
      Task classes = project.getTasks().findByName("classes");
      if (classes != null) {
        classes.dependsOn(minifyTask);
      }
    });
  }
}
