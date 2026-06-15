package com.webforj.plugin.foundation;

import java.util.Collection;

/**
 * The line oriented wire format the watch uses to forward its output to the application.
 *
 * <p>
 * The watch sends two kinds of line over the socket. A log line carries one line of bundler output
 * for the application to re-log, and a rebuild line carries the served paths that changed so the
 * application can drive the browser. Both sides reference these prefixes from this one class, so
 * the format is defined in a single place rather than copied between the sender and the receiver.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class WatchProtocol {

  /**
   * The prefix of a line that carries one line of bundler output at info level.
   */
  public static final String LOG_PREFIX = "LOG ";

  /**
   * The prefix of a line that carries one line of bundler output at warn level.
   */
  public static final String WARN_PREFIX = "WARN ";

  /**
   * The prefix of a line that carries the served paths changed by a rebuild.
   */
  public static final String REBUILD_PREFIX = "REBUILD ";

  private WatchProtocol() {}

  /**
   * Formats an info line for the given bundler output.
   *
   * @param line the bundler output line
   * @return the wire line to send
   */
  public static String log(String line) {
    return LOG_PREFIX + line;
  }

  /**
   * Formats a warn line for the given bundler output.
   *
   * @param line the bundler output line
   * @return the wire line to send
   */
  public static String warn(String line) {
    return WARN_PREFIX + line;
  }

  /**
   * Formats a rebuild line for the given changed served paths.
   *
   * @param changedPaths the served paths changed by the rebuild
   * @return the wire line to send
   */
  public static String rebuild(Collection<String> changedPaths) {
    return REBUILD_PREFIX + String.join(" ", changedPaths);
  }
}
