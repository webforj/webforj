package org.dwcj.component.events;

import java.util.HashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import org.dwcj.App;

/**
 * The EventDispatcher is minimalistic event manager that can be used to dispatch events to
 * listeners.
 *
 * @author Hyyan Abo Fakher
 */
public class EventDispatcher {

  /**
   * The listeners.
   */
  private HashMap<Class<? extends Event<?>>, CopyOnWriteArrayList<EventListener<?>>> listeners =
      new HashMap<>();

  /**
   * Adds an event listener.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @param listener the listener
   */
  public <T extends Event<?>> void addEventListener(Class<T> eventClass,
      EventListener<T> listener) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);

    if (list == null) {
      list = new CopyOnWriteArrayList<>();
      listeners.put(eventClass, list);
    }

    list.add(listener);
  }

  /**
   * Removes an event listener.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @param listener the listener
   */
  public <T extends Event<?>> void removeEventListener(Class<T> eventClass,
      EventListener<T> listener) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);

    if (list != null) {
      App.consoleLog(list.toString());
      App.consoleLog(listener.toString());
      list.remove(listener);
      App.consoleLog(list.toString());
    }
  }

  /**
   * Gets the listeners count for the given event class.
   *
   * @param eventClass the event class
   *
   * @return the listeners count
   */
  public int getListenersCount(Class<? extends Event<?>> eventClass) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);

    if (list != null) {
      return list.size();
    }

    return 0;
  }

  /**
   * Removes all listeners.
   */
  public void clear() {
    listeners.clear();
  }

  /**
   * Dispatch event.
   *
   * @param <T> the generic type
   * @param event the event
   */
  public <T extends Event<?>> void dispatchEvent(T event) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(event.getClass());

    if (list != null && !list.isEmpty()) {
      for (EventListener<?> listener : list) {
        @SuppressWarnings("unchecked")
        EventListener<T> l = (EventListener<T>) listener;
        l.execute(event);
      }
    }
  }
}
