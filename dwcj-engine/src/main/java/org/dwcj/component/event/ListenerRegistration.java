package org.dwcj.component.event;

import java.util.List;

/**
 * Represents the registration of a listener with an event dispatcher.
 *
 * @param <T> the generic type of the event
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListenerRegistration<T extends ComponentEvent<?>> {
  private final EventDispatcher dispatcher;
  private final Class<T> eventClass;
  private final ComponentEventListener<T> listener;

  ListenerRegistration(EventDispatcher dispatcher, Class<T> eventClass,
      ComponentEventListener<T> listener) {
    this.dispatcher = dispatcher;
    this.eventClass = eventClass;
    this.listener = listener;
  }

  /**
   * Removes the listener from the dispatcher.
   */
  public void remove() {
    dispatcher.removeListener(eventClass, listener);
  }

  /**
   * Gets the count of listeners for the event class.
   *
   * @return the listeners count
   */
  public int getCount() {
    return dispatcher.getCount(eventClass);
  }

  /**
   * Checks if there are more listeners registered for the event class other than the current
   * listener.
   *
   * @return true if there are more listeners (excluding the current listener), false otherwise
   */
  public boolean hasMore() {
    List<ComponentEventListener<T>> listeners = dispatcher.getListeners(eventClass);
    int count = (int) listeners.stream().filter(l -> l != listener).count();
    if (count == 1 && dispatcher.hasListener(eventClass, listener)) {
      return false;
    }

    return count > 0;
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
  public ComponentEventListener<T> getListener() {
    return listener;
  }

  /**
   * Gets the event dispatcher associated with this listener registration.
   *
   * @return the event dispatcher
   */
  EventDispatcher getEventDispatcher() {
    return dispatcher;
  }
}
