package com.webforj.plugin.gradle;

import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.gradle.devtools.SpringDevtoolsInjection;
import com.webforj.plugin.gradle.hotswap.HotswapInjection;
import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.FileCollection;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginExtension;
import org.gradle.api.provider.Provider;
import org.gradle.api.tasks.JavaExec;
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
  private static final String PUSH_KEYS_TASK = "webforjPushKeys";
  private static final String SPRING_BOOT_PLUGIN_ID = "org.springframework.boot";

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

    project.getTasks().register(PUSH_KEYS_TASK, PushKeysTask.class, task -> {
      task.setGroup(GROUP);
      task.setDescription("Generates a push key pair and prints the configuration lines.");
    });

    project.getPlugins().withType(JavaPlugin.class,
        plugin -> wireJavaLifecycle(project, bundle, frontendTest, watch));

    project.getPlugins().withId("base", plugin -> project.getTasks().named("clean")
        .configure(task -> task.dependsOn(cleanFrontend)));

    configureHotswap(project, extension);
    configureDevtools(project);
  }

  private void configureDevtools(Project project) {
    // The devtools resolve against the application runtime classpath, so the lookup is wrapped in
    // a callable that the run task asks only when it actually starts. The packaging tasks never
    // read that classpath, so a packaged application can never contain the devtools.
    project.getPlugins().withId(SPRING_BOOT_PLUGIN_ID, applied -> project.getTasks()
        .withType(JavaExec.class).matching(task -> "bootRun".equals(task.getName())).configureEach(
            task -> task.classpath(project.files(SpringDevtoolsInjection.getCallable(project)))));
  }

  private void configureHotswap(Project project, WebforjExtension extension) {
    // The Spring Boot run task is a JavaExec, so the arguments join the fork through a provider
    // that is only asked when the task actually runs.
    project.getPlugins().withId(SPRING_BOOT_PLUGIN_ID,
        applied -> project.getTasks().withType(JavaExec.class)
            .matching(task -> "bootRun".equals(task.getName()))
            .configureEach(task -> task.getJvmArgumentProviders()
                .add(() -> hotswapArguments(project, extension, true, launcherExecutable(task)))));

    // Gretty reads its jvmArgs when the run task starts, so the arguments are appended right
    // before that in a first action of the same task. Every launch task of the runner is covered,
    // whichever one the developer uses to start the application.
    Set<String> grettyLaunchTasks = Set.of("appRun", "appRunDebug", "appStart", "appStartDebug");
    project.getPlugins().withId("org.gretty",
        applied -> project.getTasks().matching(task -> grettyLaunchTasks.contains(task.getName()))
            .configureEach(
                task -> task.doFirst("webforj hotswap", started -> appendGrettyJvmArgs(project,
                    hotswapArguments(project, extension, false, null)))));

    project.afterEvaluate(evaluated -> {
      boolean configured = extension.getHotswap().isJrebelConfigured()
          || extension.getHotswap().isHotswapAgentConfigured();
      boolean runner = project.getPluginManager().hasPlugin(SPRING_BOOT_PLUGIN_ID)
          || project.getPluginManager().hasPlugin("org.gretty");

      if (configured && !runner) {
        project.getLogger().warn(
            HotswapLaunch.getMissingRunnerWarning("the Spring Boot plugin or the Gretty plugin"));
      }
    });
  }

  private List<String> hotswapArguments(Project project, WebforjExtension extension,
      boolean springBootRunner, Path javaExecutable) {
    Object selection = project.findProperty(HotswapLaunch.SELECTION_PROPERTY);

    return HotswapInjection.create().setProject(project).setOptions(extension.getHotswap())
        .setCommandLineValue(selection == null ? null : selection.toString())
        .setSpringBootRunner(springBootRunner)
        .setBuildDirectory(project.getLayout().getBuildDirectory().get().getAsFile().toPath())
        .setJavaExecutable(javaExecutable).setLog(project.getLogger()).build().getArguments();
  }

  private static Path launcherExecutable(JavaExec task) {
    // The toolchain resolved this launcher for the fork, so the capability check runs against the
    // very virtual machine the application starts in.
    return task.getJavaLauncher().isPresent()
        ? task.getJavaLauncher().get().getExecutablePath().getAsFile().toPath()
        : null;
  }

  private void appendGrettyJvmArgs(Project project, List<String> arguments) {
    if (arguments.isEmpty()) {
      return;
    }

    Object gretty = project.getExtensions().findByName("gretty");
    if (gretty == null) {
      return;
    }

    // The Gretty types are not on the plugin classpath, they come from the build that applies
    // Gretty, so its extension is reached through its property accessors.
    try {
      Method getter = gretty.getClass().getMethod("getJvmArgs");
      List<Object> merged = new ArrayList<>();
      if (getter.invoke(gretty) instanceof List<?> current) {
        merged.addAll(current);
      }
      // A build that invokes more than one launch task runs this action once per task, and the
      // agent must enter the virtual machine exactly once.
      for (String argument : arguments) {
        if (!merged.contains(argument)) {
          merged.add(argument);
        }
      }
      gretty.getClass().getMethod("setJvmArgs", List.class).invoke(gretty, merged);
    } catch (ReflectiveOperationException e) {
      throw new GradleException(
          "could not hand the hotswap agent to the Gretty runner: " + e.getMessage(), e);
    }
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
