package com.webforj.bundle.bun;

/**
 * Receives the bundler output so the invoker decides where it lands.
 *
 * <p>
 * The bundler never picks a logging framework of its own. A build tool passes an implementation
 * that routes the output to the channel it shows by default, Maven to its console, Gradle to its
 * lifecycle log. A development watch passes one that forwards every line to the running
 * application. The same instance is handed to the bundler core and to every extension through the
 * build context.
 * </p>
 *
 * <p>
 * Implementations carry out the single {@link #log(System.Logger.Level, String)} method. The level
 * is advisory, an implementation maps it onto whatever its channel shows by default.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface BundleLogger {

  /**
   * Reports one line of bundler output at the given level.
   *
   * @param level the level of the line
   * @param message the line, already formatted
   */
  void log(System.Logger.Level level, String message);

  /**
   * Reports a formatted line at debug level, for the raw output a developer reaches for only when
   * something looks wrong.
   *
   * @param format the message format, with {@code {}} placeholders
   * @param args the format arguments
   */
  default void debug(String format, Object... args) {
    log(System.Logger.Level.DEBUG, render(format, args));
  }

  /**
   * Reports a formatted line at info level, for progress and results a developer expects to see.
   *
   * @param format the message format, with {@code {}} placeholders
   * @param args the format arguments
   */
  default void info(String format, Object... args) {
    log(System.Logger.Level.INFO, render(format, args));
  }

  /**
   * Reports a formatted line at warning level.
   *
   * @param format the message format, with {@code {}} placeholders
   * @param args the format arguments
   */
  default void warn(String format, Object... args) {
    log(System.Logger.Level.WARNING, render(format, args));
  }

  /**
   * Returns a logger backed by the platform {@link System.Logger}, the webforJ default when no
   * build tool wires its own.
   *
   * @return a platform backed logger
   */
  static BundleLogger system() {
    final System.Logger logger = System.getLogger("com.webforj.bundle.bun");

    return logger::log;
  }

  /**
   * Substitutes each {@code {}} placeholder with the next argument.
   *
   * @param format the message format
   * @param args the format arguments
   * @return the formatted message
   */
  private static String render(String format, Object... args) {
    if (args == null || args.length == 0) {
      return format;
    }

    StringBuilder out = new StringBuilder(format.length() + 16 * args.length);
    int next = 0;
    int i = 0;
    while (i < format.length()) {
      if (next < args.length && i + 1 < format.length() && format.charAt(i) == '{'
          && format.charAt(i + 1) == '}') {
        out.append(String.valueOf(args[next++]));
        i += 2;
      } else {
        out.append(format.charAt(i++));
      }
    }

    return out.toString();
  }
}
