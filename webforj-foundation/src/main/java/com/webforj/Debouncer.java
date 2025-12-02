package com.webforj;

import com.webforj.dispatcher.EventListener;
import java.util.Objects;

/**
 * A debouncer delays executing an action until a specified time has elapsed since the last call.
 *
 * <p>
 * Debouncing is useful for scenarios like search-as-you-type where you want to wait until the user
 * stops typing before executing a search.
 * </p>
 *
 * <h2>Usage Example</h2>
 *
 * <pre>{@code
 * Debouncer debounce = new Debouncer(0.3f);
 *
 * textField.onModify(e -> {
 *   debounce.run(() -> search(textField.getText()));
 * });
 * }</pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 *
 * @see Interval
 */
public final class Debouncer {

  private final float delay;
  private Interval interval;
  private Runnable pendingAction;

  /**
   * Creates a new debouncer.
   *
   * @param delay the debounce delay in seconds
   * @throws IllegalArgumentException if delay is negative
   */
  public Debouncer(float delay) {
    if (delay < 0) {
      throw new IllegalArgumentException("Delay must be >= 0");
    }
    this.delay = delay;
  }

  /**
   * Schedules the action to run after the delay.
   *
   * <p>
   * If called again before the delay elapses, the previous action is cancelled and the timer
   * restarts.
   * </p>
   *
   * @param action the action to execute
   * @throws NullPointerException if action is null
   */
  public void run(Runnable action) {
    Objects.requireNonNull(action, "Action cannot be null");
    pendingAction = action;

    if (interval != null) {
      interval.stop();
    }

    interval = createInterval(delay, e -> {
      interval.stop();
      if (pendingAction != null) {
        pendingAction.run();
        pendingAction = null;
      }
    });

    interval.start();
  }

  /**
   * Cancels any pending action.
   */
  public void cancel() {
    if (interval != null) {
      interval.stop();
    }

    pendingAction = null;
  }

  /**
   * Immediately executes the pending action if one exists.
   */
  public void flush() {
    if (pendingAction != null) {
      if (interval != null) {
        interval.stop();
      }

      Runnable action = pendingAction;
      pendingAction = null;
      action.run();
    }
  }

  /**
   * Checks if there's a pending action.
   *
   * @return {@code true} if an action is pending
   */
  public boolean isPending() {
    return pendingAction != null;
  }

  /**
   * Gets the delay in seconds.
   *
   * @return the delay
   */
  public float getDelay() {
    return delay;
  }

  /**
   * Creates an interval instance. Package-private for testing.
   */
  Interval createInterval(float delay, EventListener<Interval.ElapsedEvent> listener) {
    return new Interval(delay, listener);
  }
}
