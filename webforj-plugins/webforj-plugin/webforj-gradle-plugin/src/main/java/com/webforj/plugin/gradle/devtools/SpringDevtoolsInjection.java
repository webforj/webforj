package com.webforj.plugin.gradle.devtools;

import com.webforj.plugin.foundation.devtools.SpringDevtoolsDelivery;
import com.webforj.plugin.gradle.resolve.GradleArtifacts;
import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.file.FileCollection;

/**
 * Hands the webforJ devtools jars to the application run classpath of a Gradle run.
 *
 * <p>
 * The jars the foundation delivery computes are asked for when the application run task starts, so
 * nothing is resolved for a build that never launches the application, and the packaging tasks
 * never see the resolved jars.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SpringDevtoolsInjection {

  private SpringDevtoolsInjection() {}

  /**
   * Creates the callable the run task classpath asks for the devtools jars.
   *
   * <p>
   * The task classpath is queried more than once per build, so the resolution result is kept after
   * the first call.
   * </p>
   *
   * @param project the project the application runs from
   * @return the callable resolving the missing devtools jars
   */
  public static Callable<FileCollection> getCallable(Project project) {
    return memoize(() -> resolve(project));
  }

  /**
   * Resolves the devtools jars the application misses.
   *
   * @param project the project the application runs from
   * @return the missing devtools jars, empty when the application carries everything or has no
   *         webforJ framework on its classpath
   */
  public static FileCollection resolve(Project project) {
    try {
      return project.files(SpringDevtoolsDelivery.create()
          .setApplicationClasspath(GradleArtifacts.getApplicationClasspath(project))
          .setResolver(GradleArtifacts.getResolver(project))
          .setLog(line -> project.getLogger().lifecycle(line))
          .setDebug(line -> project.getLogger().debug(line)).build().getJars());
    } catch (IOException e) {
      throw new GradleException(e.getMessage(), e);
    }
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
}
