package org.dwcj.component.event;

import org.dwcj.component.event.sink.AbstractSink;

/**
 * EventController which handles the set and remove callback and the dispatcher.
 */
public class EventController<T extends Event<?>> {
  private EventDispatcher dispatcher;
  private final AbstractSink sink;
  private final Class<T> event;

  /**
   * Creates a new Eventhandler.
   *
   * @param sink the corresponding sink to the event
   * @param event the corresponding event to the sink
   */
  public EventController(AbstractSink sink, Class<T> event) {
    this.sink = sink;
    this.dispatcher = sink.getEventDispatcher();
    this.event = event;
  }

  /**
   * Adds a event.
   *
   * @param listener the event listener to be added
   */
  public void addEventListener(EventListener<T> listener) {
    this.dispatcher.addEventListener(event, listener);
    this.sink.setCallback();
  }

  /**
   * Removes a event.
   *
   * @param listener the event listener to be removed
   */
  public void removeEventListener(EventListener<T> listener) {
    this.dispatcher.removeEventListener(event, listener);
    if (this.dispatcher.getListenersCount(event) == 0) {
      this.sink.removeCallback();
    }
  }

  /**
   * Sets the callback for the sink if the dispatcher holds any Event of type T.
   */
  public void catchUp() {
    if (this.dispatcher.getListenersCount(event) > 0) {
      this.sink.setCallback();
    }
  }

  void setDispatcher(EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }
}
