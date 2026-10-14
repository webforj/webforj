package com.webforj.devtools.livereload.receiver;

/**
 * A receiver the live reload lifecycle owns, brought up with the reload server and torn down with
 * it.
 *
 * <p>
 * Every receiver watches one change source and pushes what it sees through the reload server. A
 * receiver whose source is absent stays inert after its start, so the lifecycle starts every
 * receiver unconditionally.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface LiveReloadReceiver {

  /**
   * Brings the receiver up, unless it is already running or its change source is absent.
   */
  void start();

  /**
   * Tears the receiver down, releasing whatever its start acquired.
   */
  void stop();

  /**
   * Indicates whether the receiver is running.
   *
   * @return {@code true} between a start and a stop
   */
  boolean isRunning();

  /**
   * Prepares the hotswap tool for an application class loader that starts serving sessions.
   * Receivers of tools that track class loaders override it.
   *
   * @param loader the application class loader
   *
   * @since 26.03
   */
  default void openClassLoader(ClassLoader loader) {}

  /**
   * Releases an application class loader from the hotswap tool after its last session ended.
   *
   * @param loader the application class loader
   *
   * @since 26.03
   */
  default void closeClassLoader(ClassLoader loader) {}
}
