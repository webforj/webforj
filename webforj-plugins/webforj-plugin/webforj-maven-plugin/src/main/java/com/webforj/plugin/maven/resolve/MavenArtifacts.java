package com.webforj.plugin.maven.resolve;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

/**
 * Maps the Maven dependency world into the neutral form the foundation logic works on.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class MavenArtifacts {

  private MavenArtifacts() {}

  /**
   * The resolved runtime classpath of the application, as the Maven build resolved it.
   *
   * @param project the current Maven project
   * @return the application classpath
   */
  public static ApplicationClasspath getApplicationClasspath(MavenProject project) {
    List<ResolvedJar> jars = new ArrayList<>();
    for (Artifact artifact : project.getArtifacts()) {
      jars.add(new ResolvedJar(artifact.getGroupId(), artifact.getArtifactId(),
          artifact.getBaseVersion(),
          artifact.getFile() != null ? artifact.getFile().toPath() : null));
    }

    return new ApplicationClasspath(jars);
  }

  /**
   * The resolver that resolves a module and its runtime dependencies through the repositories of
   * the build.
   *
   * @param system the repository system of the session
   * @param session the repository session of the build
   * @param repositories the remote repositories of the project
   * @return the resolver
   */
  public static ArtifactResolver getResolver(RepositorySystem system,
      RepositorySystemSession session, List<RemoteRepository> repositories) {
    return (groupId, artifactId, version) -> {
      Dependency dependency = new Dependency(
          new DefaultArtifact(groupId + ":" + artifactId + ":" + version), JavaScopes.RUNTIME);
      // The artifact enters the request as a dependency, not as the root, so its optional
      // dependencies are pruned the way every consumer resolves them.
      CollectRequest collect = new CollectRequest(List.of(dependency), null, repositories);
      DependencyRequest request =
          new DependencyRequest(collect, DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME));

      try {
        List<ResolvedJar> jars = new ArrayList<>();
        for (ArtifactResult resolved : system.resolveDependencies(session, request)
            .getArtifactResults()) {
          jars.add(new ResolvedJar(resolved.getArtifact().getGroupId(),
              resolved.getArtifact().getArtifactId(), resolved.getArtifact().getVersion(),
              resolved.getArtifact().getFile().toPath()));
        }

        return jars;
      } catch (DependencyResolutionException e) {
        throw new IOException("could not resolve " + groupId + ":" + artifactId + ":" + version
            + " for the application run", e);
      }
    };
  }
}
