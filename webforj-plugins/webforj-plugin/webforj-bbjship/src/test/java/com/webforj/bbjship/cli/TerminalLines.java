package com.webforj.bbjship.cli;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds the lines typed on a terminal, its environment and working directory, and writes every
 * printed line to a writer.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class TerminalLines implements BBjShipTerminal {

  private final Writer printed;
  private final Deque<String> typed = new ArrayDeque<>();
  private final Map<String, String> variables = new HashMap<>();
  private final List<String> passwordReads = new ArrayList<>();
  private Path workingDirectory = Path.of("").toAbsolutePath();

  /**
   * Creates a terminal that writes every printed line to a writer.
   *
   * @param printed the writer of the printed lines
   */
  public TerminalLines(Writer printed) {
    this.printed = printed;
  }

  /**
   * Sets the lines typed on the terminal in the order they are read.
   *
   * @param lines the typed lines
   *
   * @return this terminal
   */
  public TerminalLines setTyped(String... lines) {
    typed.clear();
    typed.addAll(List.of(lines));
    return this;
  }

  /**
   * Sets the environment variables of the terminal.
   *
   * @param variables the environment variables
   *
   * @return this terminal
   */
  public TerminalLines setVariables(Map<String, String> variables) {
    this.variables.clear();
    this.variables.putAll(variables);
    return this;
  }

  /**
   * Sets the working directory of the terminal.
   *
   * @param workingDirectory the working directory
   *
   * @return this terminal
   */
  public TerminalLines setWorkingDirectory(Path workingDirectory) {
    this.workingDirectory = workingDirectory;
    return this;
  }

  /**
   * Returns the lines read as passwords.
   *
   * @return the lines read as passwords
   */
  public List<String> getPasswordReads() {
    return List.copyOf(passwordReads);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void print(String line) {
    write(line);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void printError(String line) {
    write(line);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String readLine() {
    return typed.poll();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String readPassword() {
    String line = typed.poll();
    if (line != null) {
      passwordReads.add(line);
    }

    return line;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getVariable(String name) {
    return variables.get(name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getWorkingDirectory() {
    return workingDirectory.toString();
  }

  private void write(String line) {
    try {
      printed.write(line + "\n");
      printed.flush();
    } catch (IOException failure) {
      throw new UncheckedIOException(failure);
    }
  }
}
