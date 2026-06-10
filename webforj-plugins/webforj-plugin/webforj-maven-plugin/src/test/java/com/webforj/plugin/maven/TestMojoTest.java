package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bundle.bun.BundlerExecution;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.apache.maven.model.Build;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

class TestMojoTest {

  @Test
  void shouldAppendConfiguredTestArgsToTheRequest(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.test(any(), any())).thenReturn(0);
    TestMojo mojo = newMojo(execution, tmp);
    mojo.testArgs = new String[] {"--reporter=junit", "--reporter-outfile=target/bun-junit.xml"};

    mojo.execute();

    assertEquals(List.of("--reporter=junit", "--reporter-outfile=target/bun-junit.xml"),
        capturedRequest(execution).getTestArgs());
  }

  @Test
  void shouldDefaultToNoTestArgsWhenUnset(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.test(any(), any())).thenReturn(0);
    TestMojo mojo = newMojo(execution, tmp);

    mojo.execute();

    assertEquals(List.of(), capturedRequest(execution).getTestArgs());
  }

  private static BundlerExecution.Request capturedRequest(BundlerExecution execution)
      throws Exception {
    ArgumentCaptor<BundlerExecution.Request> captor =
        ArgumentCaptor.forClass(BundlerExecution.Request.class);
    verify(execution).test(captor.capture(), any());

    return captor.getValue();
  }

  private static TestMojo newMojo(BundlerExecution execution, Path tmp) throws Exception {
    final Path frontend = Files.createDirectories(tmp.resolve("src/main/frontend"));
    Path classes = Files.createDirectories(tmp.resolve("target/classes"));

    Build build = new Build();
    build.setOutputDirectory(classes.toString());
    MavenProject project = mock(MavenProject.class);
    when(project.getBuild()).thenReturn(build);
    when(project.getBasedir()).thenReturn(tmp.toFile());
    when(project.getArtifactId()).thenReturn("app");
    when(project.getArtifacts()).thenReturn(Set.of());

    TestMojo mojo = new TestMojo() {
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
