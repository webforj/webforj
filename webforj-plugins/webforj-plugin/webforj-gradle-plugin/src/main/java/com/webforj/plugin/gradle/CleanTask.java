package com.webforj.plugin.gradle;

import com.webforj.plugin.foundation.GeneratedFrontend;
import java.io.IOException;
import java.nio.file.Path;
import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.tasks.Internal;
import org.gradle.api.tasks.TaskAction;
import org.gradle.work.DisableCachingByDefault;

/**
 * Removes the generated frontend the bundler writes outside the build directory, so a clean leaves
 * none behind.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@DisableCachingByDefault(because = "not worth caching")
public abstract class CleanTask extends DefaultTask {

  /**
   * Source root for bundle entry files.
   *
   * @return the source root property
   */
  @Internal
  public abstract DirectoryProperty getSourceRoot();

  /**
   * Removes the generated frontend directory.
   */
  @TaskAction
  public void clean() {
    Path sourceRoot = getSourceRoot().get().getAsFile().toPath();
    try {
      GeneratedFrontend.remove(sourceRoot)
          .ifPresent(removed -> getLogger().lifecycle("removed {}", removed));
    } catch (IOException e) {
      throw new GradleException("failed to clean " + GeneratedFrontend.resolveDirectory(sourceRoot),
          e);
    }
  }
}
