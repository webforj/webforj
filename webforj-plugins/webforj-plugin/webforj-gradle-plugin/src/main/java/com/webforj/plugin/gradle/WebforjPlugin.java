package com.webforj.plugin.gradle;

import java.io.File;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.FileCollection;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginExtension;
import org.gradle.api.provider.Provider;
import org.gradle.api.tasks.SourceSet;
import org.gradle.api.tasks.TaskProvider;
import org.gradle.jvm.tasks.Jar;

/**
 * Enables the webforJ bundler for a Gradle build. Applying the plugin registers the bundle, watch,
 * frontend test, and clean tasks and binds them to the build, so an application enables the whole
 * bundler integration with a single plugin declaration.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class WebforjPlugin implements Plugin<Project> {

  private static final String GROUP = "webforj";
  private static final String BUNDLE_TASK = "webforjBundle";
  private static final String TEST_TASK = "webforjTest";
  private static final String WATCH_TASK = "webforjWatch";
  private static final String CLEAN_TASK = "webforjCleanFrontend";

  /**
   * {@inheritDoc}
   */
  @Override
  public void apply(Project project) {
    WebforjExtension extension = project.getExtensions().create(GROUP, WebforjExtension.class);
    applyConventions(project, extension);

    TaskProvider<BundleTask> bundle =
        project.getTasks().register(BUNDLE_TASK, BundleTask.class, task -> {
          configureCommon(project, extension, task);
          task.setGroup(GROUP);
          task.setDescription("Bundles the frontend sources for packaging.");
        });

    TaskProvider<TestTask> frontendTest =
        project.getTasks().register(TEST_TASK, TestTask.class, task -> {
          configureCommon(project, extension, task);
          task.onlyIf(t -> !project.hasProperty("skipTests"));
          task.setGroup(GROUP);
          task.setDescription("Runs the Bun test runner over the frontend sources.");
        });

    Provider<WatchLifecycle> watchLifecycle = project.getGradle().getSharedServices()
        .registerIfAbsent("webforjWatchLifecycle", WatchLifecycle.class, spec -> {
        });

    TaskProvider<WatchTask> watch =
        project.getTasks().register(WATCH_TASK, WatchTask.class, task -> {
          configureCommon(project, extension, task);
          task.getWatchLifecycle().set(watchLifecycle);
          task.usesService(watchLifecycle);
          task.setGroup(GROUP);
          task.setDescription("Runs the development bundle watch and forwards its output to the "
              + "running application.");
        });

    TaskProvider<CleanTask> cleanFrontend =
        project.getTasks().register(CLEAN_TASK, CleanTask.class, task -> {
          task.getSourceRoot().set(extension.getSourceRoot());
          task.setGroup(GROUP);
          task.setDescription("Removes the generated frontend the bundler writes.");
        });

    project.getPlugins().withType(JavaPlugin.class,
        plugin -> wireJavaLifecycle(project, bundle, frontendTest, watch));

    project.getPlugins().withId("base", plugin -> project.getTasks().named("clean")
        .configure(task -> task.dependsOn(cleanFrontend)));
  }

  private void applyConventions(Project project, WebforjExtension extension) {
    extension.getSourceRoot()
        .convention(project.getLayout().getProjectDirectory().dir("src/main/frontend"));
    extension.getWorkDir().convention(project.getLayout().getBuildDirectory().dir("bundle"));
    extension.getCacheDir().convention(project.getLayout()
        .dir(project.provider(() -> new File(System.getProperty("user.home"), ".webforj/bun"))));
    extension.getEager().convention(false);
  }

  private void configureCommon(Project project, WebforjExtension extension,
      AbstractBundlerTask task) {
    // The user configuration is read straight from the extension, so a new option needs no wiring
    // here. Only the inputs derived from the Gradle project are set on the task.
    task.getExtension().set(extension);
    task.getProjectName().set(project.getName());
    task.getNpmRoot().set(project.getLayout().getProjectDirectory());
  }

  private void wireJavaLifecycle(Project project, TaskProvider<BundleTask> bundle,
      TaskProvider<TestTask> frontendTest, TaskProvider<WatchTask> watch) {
    JavaPluginExtension java = project.getExtensions().getByType(JavaPluginExtension.class);
    SourceSet main = java.getSourceSets().getByName(SourceSet.MAIN_SOURCE_SET_NAME);

    FileCollection classpath = main.getOutput().getClassesDirs().plus(
        project.getConfigurations().getByName(JavaPlugin.RUNTIME_CLASSPATH_CONFIGURATION_NAME));
    // The index and the served assets are written into the compiled output directory, the same
    // directory the runtime resolves them from on the classpath, matching where the Maven build
    // places them under target/classes.
    Provider<Directory> output = project.getLayout().dir(project.provider(
        () -> main.getOutput().getClassesDirs().getFiles().stream().findFirst().orElse(null)));

    final String classesTask = main.getClassesTaskName();

    bindOutputs(bundle, classpath, output, main);
    bindOutputs(frontendTest, classpath, output, main);
    bindOutputs(watch, classpath, output, main);

    bundle.configure(task -> task.dependsOn(classesTask));
    frontendTest.configure(task -> task.dependsOn(classesTask));
    watch.configure(task -> task.dependsOn(classesTask));

    project.getTasks().withType(Jar.class).configureEach(jar -> jar.dependsOn(bundle));
    // Bind to test, not check, so a plain test run executes the frontend tests too, matching the
    // Maven plugin where the frontend tests run in the test phase. check depends on test, so check
    // and build still run them.
    project.getTasks().named(JavaPlugin.TEST_TASK_NAME)
        .configure(task -> task.dependsOn(frontendTest));
  }

  private void bindOutputs(TaskProvider<? extends AbstractBundlerTask> provider,
      FileCollection classpath, Provider<Directory> output, SourceSet main) {
    provider.configure(task -> {
      task.getProjectClasspath().from(classpath);
      task.getClassesOutputDir().set(output);
      task.getSourceScanRoots().from(main.getJava().getSrcDirs());
    });
  }
}
