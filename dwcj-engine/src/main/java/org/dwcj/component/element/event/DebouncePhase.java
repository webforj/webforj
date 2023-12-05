package org.dwcj.component.element.event;

/**
 * Defines phases for an event that is debounced.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public enum DebouncePhase {
  /**
   * Leading phase that happens immediately when the event is first triggered.
   */
  LEADING,
  /**
   * Trailing phase that is sent to the server once there have been at least one timeout period
   * since the last event of the same type.
   */
  TRAILING,

  /**
   * Both phase that is sent to the server for both leading and trailing phases.
   */
  BOTH
}
