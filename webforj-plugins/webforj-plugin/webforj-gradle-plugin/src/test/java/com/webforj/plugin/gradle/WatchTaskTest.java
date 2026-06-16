package com.webforj.plugin.gradle;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.bundle.bun.WatchSession;
import com.webforj.plugin.foundation.WatchPortFile;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class WatchTaskTest {

  /**
   * A watch task with an injectable execution, so the test drives the socket and the port file
   * without resolving a real Bun binary.
   */
  public abstract static class TestableWatchTask extends WatchTask {
    private transient BundlerExecution execution;

    void setExecution(BundlerExecution execution) {
      this.execution = execution;
    }

    @Override
    protected BundlerExecution createExecution() {
      return execution;
    }
  }

  private static TestableWatchTask task(Project project, Path projectDir, File sourceRoot) {
    TestableWatchTask task = project.getTasks().create("webforjWatchTest", TestableWatchTask.class);
    WebforjExtension extension = project.getObjects().newInstance(WebforjExtension.class);
    extension.getSourceRoot().set(sourceRoot);
    extension.getWorkDir().set(projectDir.resolve("build/bundle").toFile());
    extension.getCacheDir().set(projectDir.resolve("cache").toFile());
    extension.getPlugins().set(Map.of());

    task.getClassesOutputDir().set(projectDir.resolve("build/resources/main").toFile());
    task.getNpmRoot().set(projectDir.toFile());
    task.getProjectName().set("app");
    task.getExtension().set(extension);
    task.getWatchLifecycle().set(project.getGradle().getSharedServices()
        .registerIfAbsent("webforjWatchLifecycle", WatchLifecycle.class, spec -> {
        }));

    return task;
  }

  @Test
  void shouldSkipWhenThereIsNoSourceRoot(@TempDir Path tmp) {
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    TestableWatchTask task = task(project, tmp, new File(tmp.toFile(), "no-such-frontend"));

    assertDoesNotThrow(task::watch);
  }

  @Test
  void shouldOpenTheSocketAndWriteThePortFile(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenReturn(mock(WatchSession.class));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    TestableWatchTask task = task(project, tmp, tmp.toFile());
    task.setExecution(execution);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      task.watch();

      assertTrue(Files.exists(portFile), "the discovery file is written");
      verify(execution).watch(any(), any(), any());
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheInitialBuildIsInterrupted(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new InterruptedException("stop"));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    TestableWatchTask task = task(project, tmp, tmp.toFile());
    task.setExecution(execution);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(GradleException.class, task::watch);
      assertFalse(Files.exists(portFile), "the discovery file is removed on failure");
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheWatchFailsToStart(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new RuntimeException("boom"));
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    TestableWatchTask task = task(project, tmp, tmp.toFile());
    task.setExecution(execution);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(GradleException.class, task::watch);
      assertFalse(Files.exists(portFile), "the discovery file is removed on failure");
    } finally {
      Files.deleteIfExists(portFile);
    }
  }
}
