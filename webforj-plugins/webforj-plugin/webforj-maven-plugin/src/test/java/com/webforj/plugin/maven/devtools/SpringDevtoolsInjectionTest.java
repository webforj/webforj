package com.webforj.plugin.maven.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SpringDevtoolsInjectionTest {

  private static final String SPRING = "spring-boot-maven-plugin";
  private static final String VERSION = "26.02-SNAPSHOT";
  private static final String DEVTOOLS = "webforj-spring-devtools";
  private static final String FRAMEWORK = "webforj-foundation";

  @Test
  void shouldHandTheDevtoolsDeltaToTheSpringBootFork(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(
        Set.of(framework(), artifact("org.java-websocket", "Java-WebSocket", "1.6.0")), SPRING);
    Path devtools = tmp.resolve("webforj-spring-devtools.jar");
    AtomicReference<String> requested = new AtomicReference<>();

    newInjection(project, new Properties(), (groupId, artifactId, version) -> {
      requested.set(version);
      if (FRAMEWORK.equals(artifactId)) {
        return List.of(jar("com.webforj", FRAMEWORK, tmp.resolve("foundation.jar")),
            jar("com.fasterxml.jackson.core", "jackson-core", tmp.resolve("j.jar")));
      }

      return List.of(jar("com.webforj", DEVTOOLS, devtools),
          jar("com.fasterxml.jackson.core", "jackson-core", tmp.resolve("j2.jar")),
          jar("org.java-websocket", "Java-WebSocket", tmp.resolve("dupe.jar")));
    }).apply();

    assertEquals(VERSION, requested.get(),
        "the devtools resolve at the framework version of the application");
    assertEquals(devtools.toAbsolutePath().toString(),
        project.getProperties().getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH),
        "only the devtools delta reaches the fork, the framework tree and the application "
            + "classpath both count as present");
  }

  @Test
  void shouldDoNothingWithoutTheSpringBootPlugin() throws Exception {
    MavenProject project = newProject(Set.of(framework()));
    AtomicBoolean resolved = new AtomicBoolean();

    newInjection(project, new Properties(), (groupId, artifactId, version) -> {
      resolved.set(true);
      return List.of();
    }).apply();

    assertFalse(resolved.get(), "no resolution happens without the Spring Boot runner");
    assertNull(
        project.getProperties().getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH));
  }

  @Test
  void shouldDoNothingWithoutTheFrameworkOnTheClasspath() throws Exception {
    MavenProject project = newProject(Set.of(), SPRING);
    AtomicBoolean resolved = new AtomicBoolean();

    newInjection(project, new Properties(), (groupId, artifactId, version) -> {
      resolved.set(true);
      return List.of();
    }).apply();

    assertFalse(resolved.get(), "no resolution happens without webforJ on the classpath");
    assertNull(
        project.getProperties().getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH));
  }

  @Test
  void shouldDoNothingWhenTheApplicationCarriesEverything(@TempDir Path tmp) throws Exception {
    MavenProject project =
        newProject(Set.of(framework(), artifact("com.webforj", DEVTOOLS, VERSION)), SPRING);

    newInjection(project, new Properties(),
        (groupId, artifactId, version) -> FRAMEWORK.equals(artifactId) ? List.of()
            : List.of(jar("com.webforj", DEVTOOLS, tmp.resolve("devtools.jar"))))
        .apply();

    assertNull(
        project.getProperties().getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH),
        "an application that declares the devtools itself needs nothing added");
  }

  @Test
  void shouldAppendAfterTheExistingElements(@TempDir Path tmp) throws Exception {
    Path devtools = tmp.resolve("webforj-spring-devtools.jar");
    ArtifactResolver resolver =
        (groupId, artifactId, version) -> FRAMEWORK.equals(artifactId) ? List.of()
            : List.of(jar("com.webforj", DEVTOOLS, devtools));

    MavenProject buildConfigured = newProject(Set.of(framework()), SPRING);
    buildConfigured.getProperties().setProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH,
        "custom.jar");
    newInjection(buildConfigured, new Properties(), resolver).apply();
    assertEquals("custom.jar," + devtools.toAbsolutePath(),
        buildConfigured.getProperties()
            .getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH),
        "the build supplied elements stay first");

    MavenProject commandLineConfigured = newProject(Set.of(framework()), SPRING);
    Properties userProperties = new Properties();
    userProperties.setProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH, "custom.jar");
    newInjection(commandLineConfigured, userProperties, resolver).apply();
    assertEquals("custom.jar," + devtools.toAbsolutePath(),
        userProperties.getProperty(SpringDevtoolsInjection.SPRING_ADDITIONAL_CLASSPATH),
        "the merge lands in the command line properties that outrank the project properties");
  }

  private static SpringDevtoolsInjection newInjection(MavenProject project,
      Properties userProperties, ArtifactResolver resolver) {
    return SpringDevtoolsInjection.create().setProject(project).setUserProperties(userProperties)
        .setResolver(resolver).setLog(mock(Log.class)).build();
  }

  private static ResolvedJar jar(String groupId, String artifactId, Path file) {
    return new ResolvedJar(groupId, artifactId, VERSION, file);
  }

  private static Artifact framework() {
    return artifact("com.webforj", FRAMEWORK, VERSION);
  }

  private static Artifact artifact(String groupId, String artifactId, String version) {
    Artifact artifact = mock(Artifact.class);
    when(artifact.getGroupId()).thenReturn(groupId);
    when(artifact.getArtifactId()).thenReturn(artifactId);
    when(artifact.getBaseVersion()).thenReturn(version);

    return artifact;
  }

  private static MavenProject newProject(Set<Artifact> artifacts, String... runnerArtifactIds) {
    List<Plugin> plugins = new ArrayList<>();
    for (String artifactId : runnerArtifactIds) {
      Plugin plugin = new Plugin();
      plugin.setArtifactId(artifactId);
      plugins.add(plugin);
    }

    Properties properties = new Properties();
    MavenProject project = mock(MavenProject.class);
    when(project.getProperties()).thenReturn(properties);
    when(project.getBuildPlugins()).thenReturn(plugins);
    when(project.getArtifacts()).thenReturn(new LinkedHashSet<>(artifacts));

    return project;
  }
}
