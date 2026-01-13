package com.webforj;

import com.basis.bbj.proxies.event.BBjTimerEvent;

/**
 * Internal handler for Interval timer callbacks.
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public class IntervalHandler {
  private final Interval interval;

  IntervalHandler(Interval interval) {
    this.interval = interval;
  }

  /**
   * Handles the timer event from BBj.
   *
   * @param ev the timer event
   */
  public void handleEvent(BBjTimerEvent ev) {
    interval.dispatchElapsedEvent();
  }
}
