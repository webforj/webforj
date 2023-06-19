package org.dwcj.component.event.sink;

import org.dwcj.component.event.Event;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;

/**
 * EventSinkManager is used to manage the event listeners (add/remove) for a BBjControl sink and the
 * corresponding event.
 *
 * @param <T> the event type
 */
public class EventSinkManager<T extends Event<?>> {
  private final AbstractSink sink;
  private final Class<T> event;

  /**
   * Creates a new EventSinkManager.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public EventSinkManager(AbstractSink sink, Class<T> event) {
    this.sink = sink;
    this.event = event;
  }

  /**
   * Adds a event listener.
   *
   * @param listener The event listener to be added
   */
  public void addEventListener(EventListener<T> listener) {
    getEventDispatcher().addEventListener(event, listener);
    this.sink.setCallback();
  }

  /**
   * Removes a event listener.
   *
   * @param listener The event listener to be removed
   */
  public void removeEventListener(EventListener<T> listener) {
    EventDispatcher dispatcher = getEventDispatcher();

    dispatcher.removeEventListener(event, listener);
    if (dispatcher.getListenersCount(event) == 0) {
      this.sink.removeCallback();
    }
  }

  /**
   * Catches up the sink with the current state of the event dispatcher.
   */
  public void catchUp() {
    if (getEventDispatcher().getListenersCount(event) > 0) {
      this.sink.setCallback();
    }
  }

  /**
   * Get the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  public EventDispatcher getEventDispatcher() {
    return this.sink.getEventDispatcher();
  }
}
