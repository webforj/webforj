package com.webforj.bbjship.cli;

/**
 * The terminal a command of the BBjShip command line prints to and reads from.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface BBjShipTerminal {

  /**
   * Prints a line to the standard output.
   *
   * @param line the line without its line terminator
   */
  void print(String line);

  /**
   * Prints a line to the standard error. A terminal that has one output stream only, such as the
   * thin client terminal of BBj, writes it to that stream.
   *
   * @param line the line without its line terminator
   */
  void printError(String line);

  /**
   * Reads the next line of the standard input.
   *
   * @return the line without its line terminator, or null when the input has ended
   */
  String readLine();

  /**
   * Returns the value of an environment variable of the terminal.
   *
   * @param name the name of the variable
   *
   * @return the value, or null when the variable is not set
   */
  String getVariable(String name);

  /**
   * Returns the working directory of the terminal.
   *
   * @return the absolute path of the working directory
   */
  String getWorkingDirectory();
}
