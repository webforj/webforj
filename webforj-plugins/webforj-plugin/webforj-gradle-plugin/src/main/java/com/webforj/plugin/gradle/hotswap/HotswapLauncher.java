package com.webforj.plugin.gradle.hotswap;

import com.webforj.plugin.foundation.hotswap.HotswapAttachment;
import com.webforj.plugin.foundation.hotswap.HotswapTool;
import com.webforj.plugin.foundation.hotswap.hotswapagent.HotswapAgentAttachment;
import com.webforj.plugin.foundation.hotswap.jrebel.JrebelAttachment;
import com.webforj.plugin.gradle.hotswap.hotswapagent.HotswapAgentConfiguration;
import com.webforj.plugin.gradle.hotswap.jrebel.JrebelConfiguration;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.gradle.api.GradleException;
import org.gradle.api.logging.Logger;

/**
 * Resolves the hotswap agent arguments for the application virtual machine.
 *
 * <p>
 * This runs when the application run task starts, so nothing is resolved for a build that never
 * launches the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapLauncher {

  /** The property that selects the tool on the command line. */
  public static final String SELECTION_PROPERTY = "webforj.hotswap";
  static final String SPRING_RESTART_OFF = "-Dspring.devtools.restart.enabled=false";

  private HotswapLauncher() {}

  /**
   * Resolves the selected tool and composes its virtual machine arguments.
   *
   * @param configuration the hotswap configuration
   * @param commandLineValue the value of the selection property, or null when it was not given
   * @param springBootRunner whether the arguments go to the Spring Boot run task
   * @param buildDirectory the build directory the generated agent configuration is written into
   * @param javaExecutable the java executable of the virtual machine the run task forks, as named
   *        by the Gradle toolchain, or null when the application runs in the current one
   * @param log where progress is reported
   *
   * @return the arguments, empty when hotswap stays off
   */
  public static List<String> arguments(HotswapConfiguration configuration, String commandLineValue,
      boolean springBootRunner, Path buildDirectory, Path javaExecutable, Logger log) {
    return arguments(configuration, commandLineValue, springBootRunner, buildDirectory, log,
        Path.of(System.getProperty("user.home"), ".webforj", "hotswap-agent"), javaExecutable);
  }

  static List<String> arguments(HotswapConfiguration configuration, String commandLineValue,
      boolean springBootRunner, Path buildDirectory, Logger log, Path agentCacheRoot,
      Path javaExecutable) {
    Optional<HotswapTool> selected;
    try {
      selected = HotswapTool.select(configuredTools(configuration), commandLineValue);
    } catch (IllegalArgumentException e) {
      throw new GradleException(e.getMessage(), e);
    }

    if (commandLineValue != null && !commandLineValue.isBlank()) {
      log.lifecycle("hotswap selection from the command line: {}", commandLineValue);
    }

    if (selected.isEmpty()) {
      return List.of();
    }

    try {
      List<String> arguments = new ArrayList<>(createAttachment(configuration, selected.get(),
          buildDirectory, agentCacheRoot, javaExecutable, log).arguments());
      if (springBootRunner) {
        // A development restart would replace the very classes the tool just swapped, so the two
        // must not run together, whichever tool is attached.
        arguments.add(SPRING_RESTART_OFF);
      }

      return arguments;
    } catch (IOException e) {
      throw new GradleException(e.getMessage(), e);
    }
  }

  private static Set<HotswapTool> configuredTools(HotswapConfiguration configuration) {
    Set<HotswapTool> configured = EnumSet.noneOf(HotswapTool.class);
    if (configuration.isHotswapAgentConfigured()) {
      configured.add(HotswapTool.HOTSWAP_AGENT);
    }

    if (configuration.isJrebelConfigured()) {
      configured.add(HotswapTool.JREBEL);
    }

    return configured;
  }

  private static HotswapAttachment createAttachment(HotswapConfiguration configuration,
      HotswapTool tool, Path buildDirectory, Path agentCacheRoot, Path javaExecutable, Logger log) {
    return switch (tool) {
      case HOTSWAP_AGENT -> createHotswapAgentAttachment(configuration, buildDirectory,
          agentCacheRoot, javaExecutable, log);
      case JREBEL -> createJrebelAttachment(configuration, log);
    };
  }

  private static HotswapAttachment createHotswapAgentAttachment(HotswapConfiguration configuration,
      Path buildDirectory, Path agentCacheRoot, Path javaExecutable, Logger log) {
    HotswapAgentConfiguration agent = configuration.getHotswapAgent();

    return HotswapAgentAttachment.create().setCacheRoot(agentCacheRoot)
        .setVersion(agent.getVersion().getOrNull())
        .setOverridePath(
            agent.getPath().isPresent() ? agent.getPath().get().getAsFile().toPath() : null)
        .setConfigurationDirectory(buildDirectory.resolve("hotswap"))
        .setJavaExecutable(javaExecutable).setLog(log::lifecycle).setWarn(log::warn).build();
  }

  private static HotswapAttachment createJrebelAttachment(HotswapConfiguration configuration,
      Logger log) {
    JrebelConfiguration jrebel = configuration.getJrebel();
    if (!configuration.isJrebelConfigured() || !jrebel.getPath().isPresent()) {
      throw new GradleException("set the jrebel path in the webforj hotswap configuration, "
          + "the JRebel agent location cannot be guessed");
    }

    return JrebelAttachment.create().setPath(jrebel.getPath().get().getAsFile().toPath())
        .setLog(log::lifecycle).build();
  }
}
