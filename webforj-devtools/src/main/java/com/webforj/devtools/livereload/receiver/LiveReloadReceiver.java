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
}
