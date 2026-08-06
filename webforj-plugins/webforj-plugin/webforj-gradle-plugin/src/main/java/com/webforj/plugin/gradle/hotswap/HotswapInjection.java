package com.webforj.plugin.gradle.hotswap;

import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.gradle.resolve.GradleArtifacts;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;

/**
 * Hands the configured hotswap tool to the application virtual machine of a Gradle run.
 *
 * <p>
 * The arguments the foundation launch composes are asked for when the application run task starts,
 * so nothing is resolved for a build that never launches the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapInjection {

  private HotswapInjection() {}

  /**
   * Resolves the selected tool through the foundation launch and composes its virtual machine
   * arguments.
   *
   * @param options the hotswap configuration
   * @param commandLineValue the value of the selection property, or null when it was not given
   * @param springBootRunner whether the arguments go to the Spring Boot run task
   * @param buildDirectory the build directory the generated agent configuration is written into
   * @param javaExecutable the java executable of the virtual machine the run task forks, as named
   *        by the Gradle toolchain, or null when the application runs in the current one
   * @param project the project the application runs from
   * @param log where progress is reported
   *
   * @return the arguments, empty when hotswap stays off
   */
  public static List<String> getArguments(HotswapOptions options, String commandLineValue,
      boolean springBootRunner, Path buildDirectory, Path javaExecutable, Project project,
      Logger log) {
    return getArguments(options, commandLineValue, springBootRunner, buildDirectory, javaExecutable,
        log, null, () -> GradleArtifacts.getApplicationClasspath(project),
        GradleArtifacts.getResolver(project));
  }

  static List<String> getArguments(HotswapOptions options, String commandLineValue,
      boolean springBootRunner, Path buildDirectory, Path javaExecutable, Logger log,
      Path agentCacheRoot, Supplier<ApplicationClasspath> applicationClasspath,
      ArtifactResolver resolver) {
    List<String> composed;
    try {
      composed =
          HotswapLaunch.create().setHotswapAgentConfigured(options.isHotswapAgentConfigured())
              .setHotswapAgentVersion(options.getHotswapAgent().getVersion().getOrNull())
              .setHotswapAgentPath(options.getHotswapAgent().getPath().isPresent()
                  ? options.getHotswapAgent().getPath().get().getAsFile().toPath()
                  : null)
              .setJrebelConfigured(options.isJrebelConfigured())
              .setJrebelPath(options.getJrebel().getPath().isPresent()
                  ? options.getJrebel().getPath().get().getAsFile().toPath()
                  : null)
              .setCommandLineValue(commandLineValue).setBuildDirectory(buildDirectory)
              .setAgentCacheRoot(agentCacheRoot).setJavaExecutable(javaExecutable)
              .setApplicationClasspath(applicationClasspath).setResolver(resolver)
              .setLog(log::lifecycle).setWarn(log::warn).build().getArguments();
    } catch (IllegalArgumentException | IOException e) {
      throw new GradleException(e.getMessage(), e);
    }

    if (composed.isEmpty() || !springBootRunner) {
      return composed;
    }

    List<String> arguments = new ArrayList<>(composed);
    arguments.add(HotswapLaunch.SPRING_RESTART_OFF);

    return List.copyOf(arguments);
  }
}
