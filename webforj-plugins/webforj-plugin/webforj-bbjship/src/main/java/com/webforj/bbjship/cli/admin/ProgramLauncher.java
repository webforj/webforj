package com.webforj.bbjship.cli.admin;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/**
 * Starts a program of the operating system.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
interface ProgramLauncher {

  /**
   * Starts the program with its output discarded.
   *
   * @param command the program and its arguments
   * @param directory the working directory of the program
   *
   * @return the started program
   * @throws IOException if the program cannot be started
   */
  Process start(List<String> command, Path directory) throws IOException;
}
