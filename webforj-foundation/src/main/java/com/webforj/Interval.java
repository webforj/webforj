package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.EventObject;
import java.util.List;
import java.util.UUID;

/**
 * Represents a timer that triggers an event with a fixed time delay between each triggering.
 *
 * <p>
 * The Interval class provides a straightforward way to trigger events after a specified delay. It
 * can be started, stopped, and restarted as needed. Additionally, the Interval can support multiple
 * listeners for the elapsed event. Optimized for the webforJ framework, it offers better
 * performance compared to the standard Java timer or the Swing timer.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class Interval {
  private String key = "";
  private float delay = 0;
  private EventDispatcher dispatcher = new EventDispatcher();
  private boolean running = false;

  /**
   * Creates a new instance of the interval.
   *
   * @param delay Specifies the timeout value in seconds. Fractional seconds are allowed to
   *        millisecond resolution, but a very small timeout value will cause a flood of events
   *        faster than the program can respond to them.
   * @param listener the listener to be added to the interval
   */
  public Interval(float delay, EventListener<ElapsedEvent> listener) {
    setDelay(delay);

    if (listener != null) {
      addElapsedListener(listener);
    }
  }

  /**
   * Sets the delay of the interval.
   *
   * <p>
   * Specifies the timeout value in seconds. Fractional seconds are allowed to millisecond
   * resolution, but a very small timeout value will cause a flood of events faster than the program
   * can respond to them.
   * </p>
   *
   * <p>
   * The delay must be greater than or equal to 0. If the timer is running, the delay will be
   * updated after the timer is stopped and restarted.
   * </p>
   *
   * @param delay the delay of the interval
   */
  public void setDelay(float delay) {
    if (delay < 0) {
      throw new IllegalArgumentException("The delay must be greater than or equal to 0");
    }

    this.delay = delay;
  }

  /**
   * Gets the delay of the interval.
   *
   * @return the delay of the interval
   */
  public float getDelay() {
    return delay;
  }

  /**
   * Starts the interval.
   *
   * <p>
   * If the interval is already running, this method will do nothing.
   * </p>
   */
  public void start() {
    if (isRunning()) {
      return;
    }

    key = UUID.randomUUID().toString();
    BBjAPI api = getEnvironment().getBBjAPI();

    try {
      api.createTimer(key, BasisNumber.createBasisNumber(getDelay()), new IntervalHandler(this),
          "handleEvent");
      running = true;
    } catch (NumberFormatException | BBjException e) {
      throw new WebforjRuntimeException("Failed to create the timer with the key '" + key + "'", e);
    }
  }

  /**
   * Stops the interval.
   *
   * <p>
   * If the interval is not running, this method will do nothing.
   * </p>
   */
  public void stop() {
    if (!isRunning()) {
      return;
    }

    try {
      Environment env = getEnvironment();
      if (env != null) {
        BBjAPI api = env.getBBjAPI();
        api.removeTimer(key);
      }
      running = false;
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to remove the timer with the key '" + key + "'", e);
    } catch (Exception e) {
      // In test environments, Environment might not be available
      running = false;
    }
  }

  /**
   * Restarts the interval.
   */
  public void restart() {
    stop();
    start();
  }

  /**
   * Checks if the interval is running.
   *
   * @return true if the interval is running, false otherwise
   */
  public boolean isRunning() {
    return running;
  }

  /**
   * Adds a listener to the interval.
   *
   * @param listener the listener
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<ElapsedEvent> addElapsedListener(
      EventListener<ElapsedEvent> listener) {
    return dispatcher.addListener(ElapsedEvent.class, listener);
  }

  /**
   * Alias for {@link #addElapsedListener(EventListener)}.
   *
   * @param listener the listener
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<ElapsedEvent> onElapsed(EventListener<ElapsedEvent> listener) {
    return addElapsedListener(listener);
  }

  /**
   * Gets the list of elapsed listeners.
   *
   * @return the list of elapsed listeners
   */
  public List<EventListener<ElapsedEvent>> getElapsedListeners() {
    return dispatcher.getListeners(ElapsedEvent.class);
  }

  /**
   * Dispatches the elapsed event to listeners.
   */
  void dispatchElapsedEvent() {
    dispatcher.dispatchEvent(new ElapsedEvent(this));
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }

  /**
   * Represents a time up event.
   *
   * @author Hyyan
   * @since 24.02
   */
  public class ElapsedEvent extends EventObject {

    /**
     * Creates a new instance of the event.
     *
     * @param source the source of the event
     */
    public ElapsedEvent(Object source) {
      super(source);
    }

    /**
     * Gets the interval that triggered the event.
     *
     * @return the interval instance
     */
    public Interval getInterval() {
      return (Interval) getSource();
    }
  }
}

