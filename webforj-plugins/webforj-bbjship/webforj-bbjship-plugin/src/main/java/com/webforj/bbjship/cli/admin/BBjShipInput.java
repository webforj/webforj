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

  /**
   * The refusal that tells standard input is not available, without a closing period. The caller
   * appends its way out.
   */
  public static final String STANDARD_INPUT_REFUSAL = "Standard input is not available on Windows";

  private final BBjShipTerminal terminal;
  private String operatingSystem = System.getProperty("os.name", "");

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
   * Tells whether the command can read from standard input. BBj on Windows runs the command line
   * without standard input.
   *
   * @return true when the operating system is not Windows
   */
  public boolean isStandardInputAvailable() {
    return !operatingSystem.startsWith("Windows");
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

  /**
   * Sets the name of the operating system the command runs on. By default the name the Java runtime
   * reports.
   *
   * @param operatingSystem the name of the operating system
   */
  public void setOperatingSystem(String operatingSystem) {
    this.operatingSystem = operatingSystem;
  }
}
