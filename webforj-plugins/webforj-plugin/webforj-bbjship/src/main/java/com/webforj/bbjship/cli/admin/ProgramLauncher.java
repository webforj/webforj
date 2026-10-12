package com.webforj.bbjship.cli.admin;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.OptionalInt;

/**
 * Starts a program of the operating system and waits for it to end.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
interface ProgramLauncher {

  /**
   * Starts the program with its output discarded and waits for it to end.
   *
   * @param command the program and its arguments
   * @param directory the working directory of the program
   * @param timeout the longest time to wait for the program to end
   *
   * @return the exit code, or empty when the program did not end within the timeout
   * @throws IOException if the program cannot be started
   * @throws InterruptedException if the thread is interrupted while waiting for the program
   */
  OptionalInt start(List<String> command, Path directory, Duration timeout)
      throws IOException, InterruptedException;
}
