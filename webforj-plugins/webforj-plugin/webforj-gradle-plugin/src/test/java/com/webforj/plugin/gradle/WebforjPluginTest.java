package com.webforj.plugin.gradle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bundle.bun.BundlerExecution;
import java.util.Set;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class WebforjPluginTest {

  private Project project;

  @BeforeEach
  void setUp() {
    project = ProjectBuilder.builder().build();
    project.getPlugins().apply("java");
    project.getPlugins().apply(WebforjPlugin.class);
  }

  @Test
  void shouldRegisterTheBundlerTasks() {
    assertNotNull(project.getTasks().findByName("webforjBundle"));
    assertNotNull(project.getTasks().findByName("webforjTest"));
    assertNotNull(project.getTasks().findByName("webforjWatch"));
    assertNotNull(project.getTasks().findByName("webforjCleanFrontend"));
  }

  @Test
  void shouldGroupTheTasksUnderWebforj() {
    assertEquals("webforj", project.getTasks().getByName("webforjBundle").getGroup());
    assertEquals("webforj", project.getTasks().getByName("webforjWatch").getGroup());
  }

  @Test
  void shouldCreateTheExtensionWithDefaults() {
    WebforjExtension extension = project.getExtensions().getByType(WebforjExtension.class);

    assertFalse(extension.getEager().get(), "eager is off by default");
    assertTrue(extension.getSourceRoot().get().getAsFile().getPath().endsWith("src/main/frontend"),
        "the source root defaults under src/main/frontend");
    assertTrue(extension.getWorkDir().get().getAsFile().getPath().endsWith("bundle"),
        "the work directory defaults under the build directory");
  }

  @Test
  void shouldBindTheBundleBeforeTheJar() {
    assertTrue(dependsOn("jar", "webforjBundle"), "the jar depends on the bundle");
  }

  @Test
  void shouldBindTheFrontendTestToTheTestTask() {
    assertTrue(dependsOn("test", "webforjTest"), "the test task depends on the frontend test");
  }

  @Test
  void shouldBindTheFrontendCleanToTheClean() {
    assertTrue(dependsOn("clean", "webforjCleanFrontend"),
        "the clean removes the generated frontend");
  }

  @Test
  void shouldReadTheExtensionOverridesThroughTheTask() {
    WebforjExtension extension = project.getExtensions().getByType(WebforjExtension.class);
    extension.getPlugins().put("webforj-tailwind", "true");

    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");

    assertEquals(Boolean.TRUE,
        bundle.createRequest().getExtensionOverrides().get("webforj-tailwind"));
  }

  @Test
  void shouldCarryTheProjectNameOntoTheBundleTask() {
    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");

    assertEquals(project.getName(), bundle.getProjectName().get());
  }

  @Test
  void shouldBuildTheRuntimeAndExecution() {
    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");

    assertNotNull(bundle.createRuntime(), "the runtime is built from the resolved properties");
    assertNotNull(bundle.createExecution(), "the execution is built from its collaborators");
  }

  @Test
  void shouldBuildTheRuntimeWithAnExplicitBunBinary() {
    WebforjExtension extension = project.getExtensions().getByType(WebforjExtension.class);
    extension.getBunPath().set(project.getLayout().getProjectDirectory().file("bun"));

    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");

    assertNotNull(bundle.createRuntime(), "an explicit Bun binary is honored");
  }

  @Test
  void shouldBuildTheRequestFromTheResolvedProperties() {
    WebforjExtension extension = project.getExtensions().getByType(WebforjExtension.class);
    extension.getPlugins().put("webforj-tailwind", "true");
    extension.getExcludePackages().add("com.acme.internal");
    extension.getEager().set(true);

    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");
    BundlerExecution.Request request = bundle.createRequest();

    assertEquals(project.getName(), request.getProjectName());
    assertEquals(Boolean.TRUE, request.getExtensionOverrides().get("webforj-tailwind"));
    assertTrue(request.getExcludedPackages().contains("com.acme.internal"));
    assertTrue(request.isEager(), "the eager flag carries onto the request");
  }

  @Test
  void shouldBuildTheRequestWithoutAnyConfiguration() {
    // The webforj { } block is optional.
    AbstractBundlerTask bundle =
        (AbstractBundlerTask) project.getTasks().getByName("webforjBundle");

    BundlerExecution.Request request = bundle.createRequest();

    assertTrue(request.getExtensionOverrides().isEmpty(), "no overrides without configuration");
    assertTrue(request.getExcludedPackages() == null || request.getExcludedPackages().isEmpty(),
        "no excluded packages without configuration");
    assertNotNull(request.getBundleSourceRoot(), "the source root convention applies");
  }

  private boolean dependsOn(String taskName, String dependencyName) {
    Task task = project.getTasks().getByName(taskName);
    Set<? extends Task> dependencies = task.getTaskDependencies().getDependencies(task);

    return dependencies.stream()
        .anyMatch(dependency -> dependency.getName().equals(dependencyName));
  }
}
