package com.webforj.plugin.gradle.devtools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;
import org.gradle.api.artifacts.component.ComponentArtifactIdentifier;
import org.gradle.api.artifacts.component.ModuleComponentIdentifier;
import org.gradle.api.artifacts.result.ResolvedArtifactResult;
import org.gradle.api.file.FileCollection;
import org.junit.jupiter.api.Test;

class SpringDevtoolsClasspathTest {

  private static final String VERSION = "26.02-SNAPSHOT";

  @Test
  void shouldFindTheFrameworkVersionOnlyWhenTheFrameworkIsPresent() {
    ResolvedArtifactResult spring =
        artifact("org.springframework", "spring-core", "6.2.0", "spring.jar");
    ResolvedArtifactResult framework =
        artifact("com.webforj", "webforj-foundation", VERSION, "webforj-foundation.jar");

    assertEquals(VERSION, SpringDevtoolsClasspath.frameworkVersion(artifacts(spring, framework)));
    assertNull(SpringDevtoolsClasspath.frameworkVersion(artifacts(spring)));
  }

  @Test
  void shouldKeepOnlyTheDevtoolsDelta() {
    Set<String> present = SpringDevtoolsClasspath.moduleKeys(
        artifacts(artifact("com.webforj", "webforj-foundation", VERSION, "webforj-foundation.jar"),
            artifact("org.java-websocket", "Java-WebSocket", "1.6.0", "Java-WebSocket.jar")));
    present.addAll(SpringDevtoolsClasspath.moduleKeys(artifacts(
        artifact("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-core.jar"))));
    ResolvedArtifactResult devtools =
        artifact("com.webforj", "webforj-spring-devtools", VERSION, "webforj-spring-devtools.jar");

    List<File> additions = SpringDevtoolsClasspath.missingFiles(present,
        artifacts(devtools, artifact("org.java-websocket", "Java-WebSocket", "1.6.0", "dupe.jar"),
            artifact("com.fasterxml.jackson.core", "jackson-core", "2.21.4", "jackson-dupe.jar")));

    assertEquals(List.of(devtools.getFile()), additions,
        "a JAR the application classpath or the framework tree already carries is never added");
  }

  @Test
  void shouldResolveOnlyOnceThroughTheMemoizedCallable() throws Exception {
    AtomicInteger calls = new AtomicInteger();
    FileCollection files = mock(FileCollection.class);

    Callable<FileCollection> memoized = SpringDevtoolsClasspath.memoize(() -> {
      calls.incrementAndGet();
      return files;
    });

    assertEquals(files, memoized.call());
    assertEquals(files, memoized.call());
    assertEquals(1, calls.get(),
        "the task classpath asks more than once, the resolution runs once");
  }

  private static Set<ResolvedArtifactResult> artifacts(ResolvedArtifactResult... artifacts) {
    return new LinkedHashSet<>(List.of(artifacts));
  }

  private static ResolvedArtifactResult artifact(String group, String name, String version,
      String fileName) {
    ModuleComponentIdentifier module = mock(ModuleComponentIdentifier.class);
    when(module.getGroup()).thenReturn(group);
    when(module.getModule()).thenReturn(name);
    when(module.getVersion()).thenReturn(version);

    ComponentArtifactIdentifier id = mock(ComponentArtifactIdentifier.class);
    when(id.getComponentIdentifier()).thenReturn(module);

    ResolvedArtifactResult artifact = mock(ResolvedArtifactResult.class);
    when(artifact.getId()).thenReturn(id);
    when(artifact.getFile()).thenReturn(new File(fileName));

    return artifact;
  }
}
