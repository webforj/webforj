package com.webforj.logging;

import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Platform-safe logger adapter for webforJ.
 *
 * <p>
 * Replaces {@code System.getLogger()} (Java 9+ Platform Logging API) which is not available on
 * Android. Delegates to {@code java.util.logging.Logger} which is available on all Java platforms
 * including Android/ART.
 *
 * <p>
 * Usage (drop-in replacement):
 *
 * <pre>{@code
 * // Before:
 * private static final System.Logger logger = System.getLogger(MyClass.class.getName());
 * logger.log(System.Logger.Level.INFO, "message {0}", arg);
 *
 * // After:
 * private static final WebforjLogger logger = WebforjLogger.getLogger(MyClass.class.getName());
 * logger.log(WebforjLogger.Level.INFO, "message {0}", arg);
 * }</pre>
 */
public final class WebforjLogger {

  /**
   * Log levels mirroring {@code System.Logger.Level} semantics. Maps to
   * {@code java.util.logging.Level} internally.
   */
  public enum Level {
    ALL(java.util.logging.Level.ALL), TRACE(java.util.logging.Level.FINER), DEBUG(
        java.util.logging.Level.FINE), INFO(java.util.logging.Level.INFO), WARNING(
            java.util.logging.Level.WARNING), ERROR(
                java.util.logging.Level.SEVERE), OFF(java.util.logging.Level.OFF);

    private final java.util.logging.Level julLevel;

    Level(java.util.logging.Level julLevel) {
      this.julLevel = julLevel;
    }

    java.util.logging.Level toJul() {
      return julLevel;
    }
  }

  private final Logger delegate;

  private WebforjLogger(String name) {
    this.delegate = Logger.getLogger(name);
  }

  /**
   * Returns a logger for the given name.
   *
   * @param name logger name (typically the fully qualified class name)
   * @return a WebforjLogger instance
   */
  public static WebforjLogger getLogger(String name) {
    return new WebforjLogger(name);
  }

  /**
   * Returns whether a message of the given level would be logged.
   *
   * @param level the log level to check
   * @return true if the level is currently loggable
   */
  public boolean isLoggable(Level level) {
    return delegate.isLoggable(level.toJul());
  }

  /**
   * Logs a message at the given level.
   *
   * @param level the log level
   * @param message the message (may contain {@code {0}}, {@code {1}} placeholders)
   * @param params optional parameters for message formatting
   */
  public void log(Level level, String message, Object... params) {
    if (!delegate.isLoggable(level.toJul())) {
      return;
    }
    if (params.length > 0) {
      // Check if last param is a Throwable (common pattern for error logging)
      Object last = params[params.length - 1];
      if (last instanceof Throwable) {
        if (params.length == 1) {
          delegate.log(level.toJul(), message, (Throwable) last);
        } else {
          String formatted = formatMessage(message, params);
          delegate.log(level.toJul(), formatted, (Throwable) last);
        }
      } else {
        delegate.log(level.toJul(), formatMessage(message, params));
      }
    } else {
      delegate.log(level.toJul(), message);
    }
  }

  private static String formatMessage(String message, Object... params) {
    try {
      return MessageFormat.format(message, params);
    } catch (Exception e) {
      return message;
    }
  }
}
