package com.webforj.plugin.maven.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HotswapInjectionTest {

  private static final String SPRING = "spring-boot-maven-plugin";
  private static final String JETTY = "jetty-ee11-maven-plugin";

  @Test
  void shouldHandTheAgentToTheSpringBootFork(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.contains("-agentpath:"), "the agent flag reaches the fork arguments");
    assertTrue(arguments.contains(HotswapInjection.SPRING_RESTART_OFF),
        "the development restart cannot race the redefinition");
  }

  @Test
  void shouldAppendToTheProjectForkArguments(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    project.getProperties().setProperty(HotswapInjection.SPRING_JVM_ARGUMENTS, "-Xmx1g");

    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.startsWith("-Xmx1g "), "the build supplied arguments stay first");
    assertTrue(arguments.contains("-agentpath:"));
  }

  @Test
  void shouldMergeIntoTheCommandLineValueSoItStaysVisible(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    Properties userProperties = new Properties();
    userProperties.setProperty(HotswapInjection.SPRING_JVM_ARGUMENTS, "-Xmx1g");

    newInjection(project, userProperties, jrebelOptions(tmp), null).apply();

    String merged = userProperties.getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(merged.startsWith("-Xmx1g "), "the command line arguments stay first");
    assertTrue(merged.contains("-agentpath:"),
        "the agent lands in the command line properties that outrank the project properties");
  }

  @Test
  void shouldForkTheJettyRunnerAndHandTheAgentToIt(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, JETTY);
    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    assertEquals("FORK", project.getProperties().getProperty(HotswapInjection.JETTY_DEPLOY_MODE));
    assertTrue(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS)
        .contains("-agentpath:"));
  }

  @Test
  void shouldNeverAttachToAnUnforkedJettyRunner(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, JETTY);
    Properties userProperties = new Properties();
    userProperties.setProperty(HotswapInjection.JETTY_DEPLOY_MODE, "EMBED");

    newInjection(project, userProperties, jrebelOptions(tmp), null).apply();

    // An unforked runner shares the build process, and the agent must never enter that one.
    assertNull(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS));
    assertEquals("EMBED", userProperties.getProperty(HotswapInjection.JETTY_DEPLOY_MODE));
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), jrebelOptions(tmp), "off").apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
  }

  @Test
  void shouldStayOffWithoutAnyConfiguration(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), null, null).apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue(@TempDir Path tmp) {
    MavenProject project = newProject(tmp, SPRING);

    assertThrows(MojoExecutionException.class,
        () -> newInjection(project, new Properties(), null, "dcevm").apply());
  }

  @Test
  void shouldRequireTheJrebelPath(@TempDir Path tmp) {
    HotswapOptions options = new HotswapOptions().setJrebel(new JrebelOptions());
    MavenProject project = newProject(tmp, SPRING);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class,
        () -> newInjection(project, new Properties(), options, null).apply());

    assertTrue(failure.getMessage().contains("jrebel path"));
  }

  @Test
  void shouldWarnWhenTheBuildHasNoSupportedRunner(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp);

    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
    assertNull(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS));
  }

  private static HotswapInjection newInjection(MavenProject project, Properties userProperties,
      HotswapOptions options, String commandLineValue) {
    return HotswapInjection.create().setProject(project).setUserProperties(userProperties)
        .setOptions(options).setCommandLineValue(commandLineValue).setLog(mock(Log.class)).build();
  }

  private static HotswapOptions jrebelOptions(Path tmp) throws Exception {
    Path library = tmp.resolve("libjrebel64.dylib");
    if (!Files.exists(library)) {
      Files.createFile(library);
    }

    return new HotswapOptions().setJrebel(new JrebelOptions().setPath(library.toFile()));
  }

  private static MavenProject newProject(Path tmp, String... runnerArtifactIds) {
    Build build = new Build();
    build.setDirectory(tmp.resolve("target").toString());

    List<Plugin> plugins = new ArrayList<>();
    for (String artifactId : runnerArtifactIds) {
      Plugin plugin = new Plugin();
      plugin.setArtifactId(artifactId);
      plugins.add(plugin);
    }

    Properties properties = new Properties();
    MavenProject project = mock(MavenProject.class);
    when(project.getBuild()).thenReturn(build);
    when(project.getProperties()).thenReturn(properties);
    when(project.getBuildPlugins()).thenReturn(plugins);

    return project;
  }
}
