package com.webforj.plugin.gradle;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bundle.bun.BundlerExecution;
import java.nio.file.Path;
import java.util.Map;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleTaskTest {

  /**
   * A bundle task with an injectable execution, so the test drives the task action without
   * resolving a real Bun binary or running a build.
   */
  public abstract static class TestableBundleTask extends BundleTask {
    private transient BundlerExecution execution;

    void setExecution(BundlerExecution execution) {
      this.execution = execution;
    }

    @Override
    protected BundlerExecution createExecution() {
      return execution;
    }
  }

  private static TestableBundleTask task(Project project, Path dir, BundlerExecution execution) {
    TestableBundleTask task =
        project.getTasks().create("webforjBundleTest", TestableBundleTask.class);
    task.getClassesOutputDir().set(dir.resolve("build/classes/java/main").toFile());
    task.getNpmRoot().set(dir.toFile());
    task.getProjectName().set("app");
    task.getExtension().set(extension(project, dir));
    task.setExecution(execution);

    return task;
  }

  private static WebforjExtension extension(Project project, Path dir) {
    WebforjExtension extension = project.getObjects().newInstance(WebforjExtension.class);
    extension.getSourceRoot().set(dir.resolve("src/main/frontend").toFile());
    extension.getWorkDir().set(dir.resolve("build/bundle").toFile());
    extension.getCacheDir().set(dir.resolve("cache").toFile());
    extension.getPlugins().set(Map.of());

    return extension;
  }

  @Test
  void shouldRunTheBundleWhenThereIsOutput(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.run(any(), any())).thenReturn(tmp.resolve("static/frontend"));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();

    assertDoesNotThrow(task(project, tmp, execution)::bundle);
    verify(execution).run(any(), any());
  }

  @Test
  void shouldNotFailWhenNothingIsBundled(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.run(any(), any())).thenReturn(null);
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();

    assertDoesNotThrow(task(project, tmp, execution)::bundle);
  }

  @Test
  void shouldWrapAnInterruptedBundle(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.run(any(), any())).thenThrow(new InterruptedException("stop"));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();

    GradleException error =
        assertThrows(GradleException.class, task(project, tmp, execution)::bundle);
    assertTrue(error.getMessage().contains("interrupted"));
  }

  @Test
  void shouldWrapAFailedBundle(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.run(any(), any())).thenThrow(new RuntimeException("boom"));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();

    GradleException error =
        assertThrows(GradleException.class, task(project, tmp, execution)::bundle);
    assertTrue(error.getMessage().contains("bundle failed"));
  }
}
