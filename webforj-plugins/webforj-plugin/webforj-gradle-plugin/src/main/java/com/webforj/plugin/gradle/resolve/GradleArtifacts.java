package com.webforj.plugin.gradle.resolve;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.component.ModuleComponentIdentifier;
import org.gradle.api.artifacts.result.ResolvedArtifactResult;

/**
 * Maps the Gradle dependency world into the neutral form the foundation logic works on.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class GradleArtifacts {

  private static final String RUNTIME_CLASSPATH = "runtimeClasspath";

  private GradleArtifacts() {}

  /**
   * The resolved runtime classpath of the application, as the Gradle build resolved it.
   *
   * @param project the project the application runs from
   * @return the application classpath
   */
  public static ApplicationClasspath getApplicationClasspath(Project project) {
    return new ApplicationClasspath(getJars(project.getConfigurations().getByName(RUNTIME_CLASSPATH)
        .getIncoming().getArtifacts().getArtifacts()));
  }

  /**
   * The resolver that resolves a module and its runtime dependencies through the repositories of
   * the build.
   *
   * @param project the project the application runs from
   * @return the resolver
   */
  public static ArtifactResolver getResolver(Project project) {
    return (groupId, artifactId, version) -> {
      Configuration configuration = project.getConfigurations().detachedConfiguration(
          project.getDependencies().create(groupId + ":" + artifactId + ":" + version));

      try {
        return getJars(configuration.getIncoming().getArtifacts().getArtifacts());
      } catch (RuntimeException e) {
        throw new IOException("could not resolve " + groupId + ":" + artifactId + ":" + version
            + " for the application run", e);
      }
    };
  }

  static List<ResolvedJar> getJars(Set<ResolvedArtifactResult> artifacts) {
    List<ResolvedJar> jars = new ArrayList<>();
    for (ResolvedArtifactResult artifact : artifacts) {
      if (artifact.getId().getComponentIdentifier() instanceof ModuleComponentIdentifier module) {
        jars.add(new ResolvedJar(module.getGroup(), module.getModule(), module.getVersion(),
            artifact.getFile().toPath()));
      }
    }

    return jars;
  }
}
