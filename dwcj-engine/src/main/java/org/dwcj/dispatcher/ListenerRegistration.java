package org.dwcj.dispatcher;

import java.util.EventObject;

/**
 * Represents the registration of a listener with an event dispatcher.
 *
 * @param <T> the generic type of the event
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListenerRegistration<T extends EventObject> {
  private final EventDispatcher dispatcher;
  private final Class<T> eventClass;
  private final EventListener<T> listener;
  private boolean removed = false;

  protected ListenerRegistration(EventDispatcher dispatcher, Class<T> eventClass,
      EventListener<T> listener) {
    this.dispatcher = dispatcher;
    this.eventClass = eventClass;
    this.listener = listener;
  }

  /**
   * Removes the listener from the dispatcher.
   */
  public void remove() {
    if (removed) {
      return;
    }

    dispatcher.removeListener(eventClass, listener);
    removed = true;
  }

  /**
   * Gets the event class associated with this listener registration.
   *
   * @return the event class
   */
  public Class<T> getEventClass() {
    return eventClass;
  }

  /**
   * Gets the listener associated with this listener registration.
   *
   * @return the listener
   */
  public EventListener<T> getListener() {
    return listener;
  }

  /**
   * Gets the event dispatcher associated with this listener registration.
   *
   * @return the event dispatcher
   */
  public EventDispatcher getEventDispatcher() {
    return dispatcher;
  }
}
