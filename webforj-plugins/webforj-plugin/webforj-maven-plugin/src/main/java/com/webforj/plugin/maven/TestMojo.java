package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundlerExecution;
import java.util.Arrays;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Goal that runs the Bun test runner over the frontend sources.
 *
 * <p>
 * Bound to {@code test} so frontend unit tests run in the same Maven test phase as the Java tests.
 * A non zero Bun exit fails the build.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Mojo(name = "test", defaultPhase = LifecyclePhase.TEST,
    requiresDependencyResolution = ResolutionScope.TEST, threadSafe = false)
public class TestMojo extends AbstractBundlerMojo {

  /** Skip frontend tests, honoring the standard {@code skipTests} property. */
  @Parameter(property = "skipTests", defaultValue = "false")
  private boolean skipTests;

  /** Skip the whole test phase, honoring the standard {@code maven.test.skip} property. */
  @Parameter(property = "maven.test.skip", defaultValue = "false")
  private boolean mavenTestSkip;

  /**
   * Extra arguments appended to the {@code bun test} invocation.
   *
   * <p>
   * For example {@code --reporter=junit} together with an absolute {@code --reporter-outfile} path
   * writes a JUnit report for a continuous integration server. A path that writes a file must be
   * absolute, such as one under {@code project.build.directory}, because Bun runs from the frontend
   * source root and does not create the directory itself.
   * </p>
   */
  @Parameter(property = "webforj.bundler.testArgs")
  protected String[] testArgs;

  @Override
  public void execute() throws MojoExecutionException {
    if (skipTests || mavenTestSkip) {
      getLog().info("webforj-bun tests skipped");

      return;
    }

    BundlerExecution execution = createExecution();
    try {
      int code = execution
          .test(createRequest().setTestArgs(testArgs == null ? null : Arrays.asList(testArgs)));
      if (code != 0) {
        throw new MojoExecutionException("bun test failed with exit code " + code);
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new MojoExecutionException("bun test was interrupted", e);
    } catch (MojoExecutionException e) {
      throw e;
    } catch (Exception e) {
      throw new MojoExecutionException("bun test failed: " + e.getMessage(), e);
    }
  }
}
