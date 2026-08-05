package com.webforj.plugin.foundation.hotswap;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The class redefinition tools the build plugins can attach to the application virtual machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum HotswapTool {

  /** The HotswapAgent java agent, downloaded by the build or pointed at by the configuration. */
  HOTSWAP_AGENT("hotswapAgent"),

  /** The JRebel agent, pointed at by the application configuration. */
  JREBEL("jrebel");

  /** The command line value that switches hotswap off for the run. */
  public static final String OFF = "off";

  private final String value;

  HotswapTool(String value) {
    this.value = value;
  }

  /**
   * The name of this tool as it appears in the build configuration and the selection property.
   *
   * @return the configuration value
   */
  public String getValue() {
    return value;
  }

  /**
   * Resolves the tool to attach, from the project configuration and the command line selection.
   *
   * <p>
   * The project configuration names at most one tool. The command line selection, when present,
   * wins over the project configuration, so a developer can try a tool without touching the build
   * file or switch one off for a single run.
   * </p>
   *
   * @param configured the tools named in the project configuration
   * @param commandLineValue the value of the selection property, or null when it was not given
   *
   * @return the tool to attach, or empty when hotswap stays off
   * @throws IllegalArgumentException if the project configuration names more than one tool, or the
   *         command line value names no known tool
   */
  public static Optional<HotswapTool> select(Set<HotswapTool> configured, String commandLineValue) {
    if (configured.size() > 1) {
      throw new IllegalArgumentException(
          "configure exactly one hotswap tool, the build names " + configured.stream()
              .map(HotswapTool::getValue).sorted().collect(Collectors.joining(" and ")));
    }

    if (commandLineValue == null || commandLineValue.isBlank()) {
      return configured.stream().findFirst();
    }

    if (OFF.equals(commandLineValue)) {
      return Optional.empty();
    }

    for (HotswapTool tool : values()) {
      if (tool.value.equals(commandLineValue)) {
        return Optional.of(tool);
      }
    }

    throw new IllegalArgumentException("unknown hotswap selection '" + commandLineValue + "', use "
        + Stream.of(values()).map(HotswapTool::getValue).collect(Collectors.joining(", ")) + " or "
        + OFF);
  }
}
