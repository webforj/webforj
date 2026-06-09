package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.plugin.foundation.WatchPortFile;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class WatchMojoTest {

  @Test
  void shouldSkipWhenThereIsNoSourceRoot() {
    WatchMojo mojo = new WatchMojo();
    mojo.sourceRoot = new File("target/no-such-frontend-directory");

    assertDoesNotThrow(mojo::execute);
  }

  @Test
  void shouldOpenTheSocketAndWriteThePortFile(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenReturn(mock(Process.class));
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      mojo.execute();

      assertTrue(Files.exists(portFile));
      verify(execution).watch(any(), any(), any());
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheInitialBuildIsInterrupted(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new InterruptedException("stop"));
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(MojoExecutionException.class, mojo::execute);
      assertFalse(Files.exists(portFile));
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheWatchFailsToStart(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new RuntimeException("boom"));
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(MojoExecutionException.class, mojo::execute);
      assertFalse(Files.exists(portFile));
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  private static WatchMojo newMojo(BundlerExecution execution, Path tmp) throws Exception {
    final Path frontend = Files.createDirectories(tmp.resolve("src/main/frontend"));
    Path classes = Files.createDirectories(tmp.resolve("target/classes"));

    Build build = new Build();
    build.setOutputDirectory(classes.toString());
    MavenProject project = mock(MavenProject.class);
    when(project.getBuild()).thenReturn(build);
    when(project.getBasedir()).thenReturn(tmp.toFile());
    when(project.getArtifactId()).thenReturn("app");
    when(project.getArtifacts()).thenReturn(Set.of());

    WatchMojo mojo = new WatchMojo() {
      @Override
      protected BundlerExecution createExecution() {
        return execution;
      }
    };
    mojo.project = project;
    mojo.sourceRoot = frontend.toFile();
    mojo.workDir = tmp.resolve("target/bundle").toFile();

    return mojo;
  }
}
