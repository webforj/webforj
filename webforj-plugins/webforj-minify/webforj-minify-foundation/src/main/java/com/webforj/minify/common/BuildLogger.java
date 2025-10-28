package com.webforj.minify.common;

/**
 * Abstraction for build tool logging, allowing both Maven and Gradle plugins to use the same core
 * logic.
 *
 * @author Kevin Hagel
 */
public interface BuildLogger {

  /**
   * Log an informational message.
   *
   * @param message the message to log
   */
  void info(String message);

  /**
   * Log a warning message.
   *
   * @param message the message to log
   */
  void warn(String message);

  /**
   * Log a debug message.
   *
   * @param message the message to log
   */
  void debug(String message);

  /**
   * Log an error message.
   *
   * @param message the message to log
   * @param throwable the exception associated with the error
   */
  void error(String message, Throwable throwable);
}
