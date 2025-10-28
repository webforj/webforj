package com.webforj.minify.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
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

    // Get source sets
    SourceSetContainer sourceSets =
        project.getExtensions().getByType(SourceSetContainer.class);
    SourceSet mainSourceSet = sourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME);

    // Register minify task
    var minifyTask = project.getTasks().register("minify", MinifyTask.class, task -> {
      // Configure task inputs
      task.getOutputDirectory().set(mainSourceSet.getOutput().getClassesDirs().getSingleFile());
      task.getResourcesDirectory()
          .set(mainSourceSet.getResources().getSourceDirectories().getSingleFile());
      task.getSkip().set(extension.getSkip());

      // Run after classes task
      task.dependsOn(project.getTasks().named("classes"));
    });

    // Make jar/war task depend on minify (must be done outside register block)
    project.getTasks().named("jar").configure(jarTask -> jarTask.dependsOn(minifyTask));

    // Also configure war task if present
    project.getPlugins().withId("war", plugin -> {
      project.getTasks().named("war").configure(warTask -> warTask.dependsOn(minifyTask));
    });

    project.getLogger().debug("webforJ Minify Plugin applied to project: {}", project.getName());
  }
}
