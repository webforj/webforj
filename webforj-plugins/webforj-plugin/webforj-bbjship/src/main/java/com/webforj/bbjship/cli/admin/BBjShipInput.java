package com.webforj.bbjship.cli.admin;

import com.webforj.bbjship.cli.BBjShipTerminal;
import java.util.Optional;

/**
 * Reads what a BBjShip command takes from the terminal that runs it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipInput {

  private final BBjShipTerminal terminal;

  /**
   * Creates the input of a terminal.
   *
   * @param terminal the terminal that runs the command
   */
  public BBjShipInput(BBjShipTerminal terminal) {
    this.terminal = terminal;
  }

  /**
   * Reads the next line the terminal types.
   *
   * @return the line without its line terminator, empty when the input has ended
   */
  public Optional<String> readLine() {
    return Optional.ofNullable(terminal.readLine());
  }

  /**
   * Prints a prompt and reads the password the terminal types.
   *
   * @param prompt the prompt line
   *
   * @return the password, empty when the input has ended
   */
  public Optional<String> readPassword(String prompt) {
    terminal.print(prompt);

    return Optional.ofNullable(terminal.readPassword());
  }

  /**
   * Finds an environment variable of the terminal.
   *
   * @param name the name of the variable
   *
   * @return the value, empty when the variable is not set
   */
  public Optional<String> findVariable(String name) {
    return Optional.ofNullable(terminal.getVariable(name));
  }
}
