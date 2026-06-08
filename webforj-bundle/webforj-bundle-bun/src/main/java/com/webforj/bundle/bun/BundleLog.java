package com.webforj.bundle.bun;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import org.slf4j.Logger;
import org.slf4j.event.Level;
import org.slf4j.helpers.MessageFormatter;

/**
 * Routes the bundler progress logging either to slf4j or to a line sink that forwards it elsewhere.
 *
 * <p>
 * A single shot build logs through slf4j, so its progress appears on the build console. A
 * development watch sets a sink, so every line is forwarded to the running application and nothing
 * is written to the build console while the application is up.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleLog {

  private final Logger slf4j;
  private final AtomicReference<BiConsumer<Level, String>> sink = new AtomicReference<>();

  BundleLog(Logger slf4j) {
    this.slf4j = slf4j;
  }

  /**
   * Routes subsequent lines to the given sink with their level, or back to slf4j when {@code null}.
   *
   * @param sink the line sink, or {@code null} to log through slf4j
   */
  void setSink(BiConsumer<Level, String> sink) {
    this.sink.set(sink);
  }

  /**
   * Logs a formatted line at info level, to the sink when one is set, otherwise through slf4j.
   *
   * @param format the slf4j style message format
   * @param args the format arguments
   */
  public void info(String format, Object... args) {
    BiConsumer<Level, String> current = sink.get();
    if (current != null) {
      current.accept(Level.INFO, MessageFormatter.arrayFormat(format, args).getMessage());
    } else {
      slf4j.info(format, args);
    }
  }

  /**
   * Logs a formatted line at warn level, to the sink when one is set, otherwise through slf4j.
   *
   * @param format the slf4j style message format
   * @param args the format arguments
   */
  public void warn(String format, Object... args) {
    BiConsumer<Level, String> current = sink.get();
    if (current != null) {
      current.accept(Level.WARN, MessageFormatter.arrayFormat(format, args).getMessage());
    } else {
      slf4j.warn(format, args);
    }
  }
}
