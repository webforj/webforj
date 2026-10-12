package com.webforj.plugin.maven.resolve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class MavenArtifactsTest {

  @Test
  void shouldLeaveProvidedAndTestArtifactsOutOfTheRuntimeClasspath() {
    MavenProject project = new MavenProject();
    project.setArtifacts(Set.of(createArtifact("webforj-foundation", "compile"),
        createArtifact("webforj-data", "runtime"), createArtifact("webforj-engine", "provided"),
        createArtifact("webforj-testbed", "test")));

    List<String> jars = MavenArtifacts.getRuntimeClasspath(project).getJars().stream()
        .map(ResolvedJar::artifactId).sorted().toList();

    assertEquals(List.of("webforj-data", "webforj-foundation"), jars);
  }

  private static Artifact createArtifact(String artifactId, String scope) {
    Artifact artifact = new org.apache.maven.artifact.DefaultArtifact("com.webforj", artifactId,
        "26.03-SNAPSHOT", scope, "jar", null, new DefaultArtifactHandler("jar"));
    artifact.setFile(Path.of(artifactId + ".jar").toFile());

    return artifact;
  }

  @Test
  void shouldResolveTheBbjShipPackageOfTheVersion() throws Exception {
    RepositorySystem system = mock(RepositorySystem.class);
    RepositorySystemSession session = mock(RepositorySystemSession.class);
    List<RemoteRepository> repositories = List.of(
        new RemoteRepository.Builder("releases", "default", "https://repo.example.test/releases")
            .build());
    Path bundle = Path.of("webforj-bbjship-26.03-SNAPSHOT-bbj-plugin.zip");
    when(system.resolveArtifact(eq(session), any()))
        .thenReturn(new ArtifactResult(new ArtifactRequest()).setArtifact(
            new DefaultArtifact("com.webforj:webforj-bbjship:zip:bbj-plugin:26.03-SNAPSHOT")
                .setFile(bundle.toFile())));

    Path resolved =
        MavenArtifacts.resolveBBjShipPackage(system, session, repositories, "26.03-SNAPSHOT");

    ArgumentCaptor<ArtifactRequest> request = ArgumentCaptor.forClass(ArtifactRequest.class);
    verify(system).resolveArtifact(eq(session), request.capture());
    assertEquals("com.webforj:webforj-bbjship:zip:bbj-plugin:26.03-SNAPSHOT",
        request.getValue().getArtifact().toString());
    assertEquals(repositories, request.getValue().getRepositories());
    assertEquals(bundle, resolved);
  }
}
