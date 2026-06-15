package com.webforj.plugin.gradle;

import com.webforj.bundle.bun.BundlerExecution;
import java.nio.file.Path;
import org.gradle.api.GradleException;
import org.gradle.api.tasks.TaskAction;
import org.gradle.work.DisableCachingByDefault;

/**
 * Bundles the project sources for a single build, so the Bun output is packaged with the
 * application.
 *
 * <p>
 * The bundler runs entirely on this plugin's classpath, so no bundler class is ever placed on the
 * application classpath or shipped with the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@DisableCachingByDefault(because = "not worth caching")
public abstract class BundleTask extends AbstractBundlerTask {

  /**
   * Runs the production bundle.
   */
  @TaskAction
  public void bundle() {
    BundlerExecution execution = createExecution();
    try {
      Path outputDir = execution.run(createRequest(), new GradleBundleLogger(getLogger()));
      if (outputDir != null) {
        getLogger().lifecycle("bundle output written to {}", outputDir);
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new GradleException("bundle was interrupted", e);
    } catch (Exception e) {
      throw new GradleException("bundle failed: " + e.getMessage(), e);
    }
  }
}
