package com.webforj.plugin.gradle;

import com.webforj.bundle.bun.BundlerExecution;
import java.util.List;
import org.gradle.api.GradleException;
import org.gradle.api.tasks.TaskAction;
import org.gradle.work.DisableCachingByDefault;

/**
 * Runs the Bun test runner over the frontend sources. A non zero Bun exit fails the build.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@DisableCachingByDefault(because = "not worth caching")
public abstract class TestTask extends AbstractBundlerTask {

  /**
   * Runs the frontend tests.
   */
  @TaskAction
  public void test() {
    List<String> testArgs = getExtension().get().getTestArgs().getOrElse(List.of());
    BundlerExecution execution = createExecution();
    try {
      int code = execution.test(createRequest().setTestArgs(testArgs),
          new GradleBundleLogger(getLogger()));
      if (code != 0) {
        throw new GradleException("bun test failed with exit code " + code);
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new GradleException("bun test was interrupted", e);
    } catch (GradleException e) {
      throw e;
    } catch (Exception e) {
      throw new GradleException("bun test failed: " + e.getMessage(), e);
    }
  }
}
