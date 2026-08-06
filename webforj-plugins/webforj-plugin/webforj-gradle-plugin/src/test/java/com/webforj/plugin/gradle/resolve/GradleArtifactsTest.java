package com.webforj.plugin.gradle.resolve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.File;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.gradle.api.artifacts.component.ComponentArtifactIdentifier;
import org.gradle.api.artifacts.component.ModuleComponentIdentifier;
import org.gradle.api.artifacts.component.ProjectComponentIdentifier;
import org.gradle.api.artifacts.result.ResolvedArtifactResult;
import org.junit.jupiter.api.Test;

class GradleArtifactsTest {

  @Test
  void shouldMapTheModuleArtifactsIntoTheNeutralForm() {
    ResolvedArtifactResult spring =
        artifact("org.springframework", "spring-core", "6.2.0", "spring.jar");
    ResolvedArtifactResult framework =
        artifact("com.webforj", "webforj-foundation", "26.02-SNAPSHOT", "webforj-foundation.jar");

    List<ResolvedJar> jars = GradleArtifacts.getJars(artifacts(spring, framework));

    assertEquals(List.of(
        new ResolvedJar("org.springframework", "spring-core", "6.2.0", Path.of("spring.jar")),
        new ResolvedJar("com.webforj", "webforj-foundation", "26.02-SNAPSHOT",
            Path.of("webforj-foundation.jar"))),
        jars);
  }

  @Test
  void shouldSkipTheArtifactThatNamesNoModule() {
    ProjectComponentIdentifier projectId = mock(ProjectComponentIdentifier.class);
    ComponentArtifactIdentifier id = mock(ComponentArtifactIdentifier.class);
    when(id.getComponentIdentifier()).thenReturn(projectId);
    ResolvedArtifactResult local = mock(ResolvedArtifactResult.class);
    when(local.getId()).thenReturn(id);

    assertEquals(List.of(), GradleArtifacts.getJars(artifacts(local)),
        "a project build output carries no module coordinates to compare by");
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
