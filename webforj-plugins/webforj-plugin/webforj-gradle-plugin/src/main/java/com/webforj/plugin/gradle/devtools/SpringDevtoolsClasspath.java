package com.webforj.plugin.gradle.devtools;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.component.ComponentIdentifier;
import org.gradle.api.artifacts.component.ModuleComponentIdentifier;
import org.gradle.api.artifacts.result.ResolvedArtifactResult;
import org.gradle.api.file.FileCollection;

/**
 * Resolves the webforJ devtools JARs for the application run classpath.
 *
 * <p>
 * The devtools are resolved at the framework version found on the application runtime classpath,
 * and only their own delta is returned, the JARs that neither the application classpath nor the
 * framework dependency tree carries, so the application keeps the framework exactly as its build
 * resolved it. This runs when the application run task starts, so nothing is resolved for a build
 * that never launches the application, and the packaging tasks never see the resolved JARs.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SpringDevtoolsClasspath {

  static final String DEVTOOLS_GROUP_ID = "com.webforj";
  static final String DEVTOOLS_ARTIFACT_ID = "webforj-spring-devtools";

  private static final String FRAMEWORK_ARTIFACT_ID = "webforj-foundation";
  private static final String RUNTIME_CLASSPATH = "runtimeClasspath";

  private SpringDevtoolsClasspath() {}

  /**
   * Creates the callable the run task classpath asks for the devtools JARs.
   *
   * <p>
   * The task classpath is queried more than once per build, so the resolution result is kept after
   * the first call.
   * </p>
   *
   * @param project the project the application runs from
   * @return the callable resolving the missing devtools JARs
   */
  public static Callable<FileCollection> callable(Project project) {
    return memoize(() -> resolve(project));
  }

  /**
   * Resolves the devtools JARs the application misses.
   *
   * @param project the project the application runs from
   * @return the missing devtools JARs, empty when the application carries everything or has no
   *         webforJ framework on its classpath
   */
  public static FileCollection resolve(Project project) {
    Set<ResolvedArtifactResult> runtime = project.getConfigurations().getByName(RUNTIME_CLASSPATH)
        .getIncoming().getArtifacts().getArtifacts();

    String version = frameworkVersion(runtime);
    if (version == null) {
      project.getLogger().debug(
          "no webforJ framework on the application classpath, the devtools delivery is skipped");

      return project.files();
    }

    // The framework tree also counts as present, whether or not the application graph carries all
    // of it, so the delivery adds only the devtools delta and never changes how the application
    // resolved the framework, for example a JAR its build deliberately excluded.
    Set<String> present = moduleKeys(runtime);
    present.addAll(moduleKeys(resolveDetached(project, FRAMEWORK_ARTIFACT_ID, version)));

    List<File> additions =
        missingFiles(present, resolveDetached(project, DEVTOOLS_ARTIFACT_ID, version));

    if (!additions.isEmpty()) {
      project.getLogger().lifecycle(
          "devtools: handing {} webforJ devtools JAR(s) to the application run classpath",
          additions.size());
    }

    return project.files(additions);
  }

  static Callable<FileCollection> memoize(Callable<FileCollection> delegate) {
    AtomicReference<FileCollection> resolved = new AtomicReference<>();

    return () -> {
      FileCollection current = resolved.get();
      if (current == null) {
        FileCollection computed = delegate.call();
        current = resolved.compareAndSet(null, computed) ? computed : resolved.get();
      }

      return current;
    };
  }

  static String frameworkVersion(Set<ResolvedArtifactResult> artifacts) {
    for (ResolvedArtifactResult artifact : artifacts) {
      ComponentIdentifier id = artifact.getId().getComponentIdentifier();
      if (id instanceof ModuleComponentIdentifier module
          && DEVTOOLS_GROUP_ID.equals(module.getGroup())
          && FRAMEWORK_ARTIFACT_ID.equals(module.getModule())) {
        return module.getVersion();
      }
    }

    return null;
  }

  static Set<String> moduleKeys(Set<ResolvedArtifactResult> artifacts) {
    Set<String> keys = new LinkedHashSet<>();
    for (ResolvedArtifactResult artifact : artifacts) {
      String key = key(artifact);
      if (key != null) {
        keys.add(key);
      }
    }

    return keys;
  }

  static List<File> missingFiles(Set<String> present, Set<ResolvedArtifactResult> devtools) {
    List<File> additions = new ArrayList<>();
    for (ResolvedArtifactResult artifact : devtools) {
      String key = key(artifact);
      if (key != null && !present.contains(key) && !additions.contains(artifact.getFile())) {
        additions.add(artifact.getFile());
      }
    }

    return additions;
  }

  private static Set<ResolvedArtifactResult> resolveDetached(Project project, String artifactId,
      String version) {
    Configuration configuration = project.getConfigurations().detachedConfiguration(
        project.getDependencies().create(DEVTOOLS_GROUP_ID + ":" + artifactId + ":" + version));

    try {
      return configuration.getIncoming().getArtifacts().getArtifacts();
    } catch (RuntimeException e) {
      throw new GradleException("could not resolve " + DEVTOOLS_GROUP_ID + ":" + artifactId + ":"
          + version + " for the application run classpath", e);
    }
  }

  private static String key(ResolvedArtifactResult artifact) {
    ComponentIdentifier id = artifact.getId().getComponentIdentifier();
    if (id instanceof ModuleComponentIdentifier module) {
      return module.getGroup() + ":" + module.getModule();
    }

    return null;
  }
}
