package org.dwcj.dispatcher;

import java.util.Collections;
import java.util.EventObject;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.BiPredicate;

/**
 * The EventDispatcher is a minimalistic event manager that can be used to dispatch component events
 * to listeners.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class EventDispatcher {

  private final Map<Class<? extends EventObject>, CopyOnWriteArrayList<EventListener<?>>> listeners =
      new ConcurrentHashMap<>();

  /**
   * Adds an event listener and returns a ListenerRegistration object containing information about
   * the listener and event type.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @param listener the listener
   *
   * @return a ListenerRegistration object
   */
  public <T extends EventObject> ListenerRegistration<T> addListener(Class<T> eventClass,
      EventListener<T> listener) {
    CopyOnWriteArrayList<EventListener<?>> list =
        listeners.computeIfAbsent(eventClass, k -> new CopyOnWriteArrayList<>());
    list.add(listener);

    return new ListenerRegistration<>(this, eventClass, listener);
  }

  /**
   * Removes an event listener.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @param listener the listener
   */
  public <T extends EventObject> void removeListener(Class<T> eventClass,
      EventListener<T> listener) {
    CopyOnWriteArrayList<EventListener<?>> list =
        listeners.computeIfAbsent(eventClass, k -> new CopyOnWriteArrayList<>());
    list.remove(listener);
  }

  /**
   * Removes all event listeners of a given type.
   *
   * @param <T> the generic type
   * @param eventClass the event class for which listeners should be removed
   */
  public <T extends EventObject> void removeAllListeners(Class<T> eventClass) {
    listeners.remove(eventClass);
  }

  /**
   * Removes all listeners.
   */
  public void removeAllListeners() {
    listeners.clear();
  }

  /**
   * Checks if a listener is registered for a specific event class.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @param listener the listener
   * @return true if the listener is registered, false otherwise
   */
  public <T extends EventObject> boolean hasListener(Class<T> eventClass,
      EventListener<T> listener) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);
    return list != null && list.contains(listener);
  }

  /**
   * Checks if there are any listeners registered for a specific event class.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @return true if there are listeners registered, false otherwise
   */
  public <T extends EventObject> boolean hasListener(Class<T> eventClass) {
    return getCount(eventClass) > 0;
  }

  /**
   * Gets the listeners for the given event class.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   *
   * @return the listeners
   */
  public <T extends EventObject> List<EventListener<T>> getListeners(Class<T> eventClass) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);

    if (list == null) {
      return Collections.emptyList();
    } else {
      @SuppressWarnings("unchecked")
      List<EventListener<T>> typedList =
          list.stream().map(listener -> (EventListener<T>) listener).toList();
      return Collections.unmodifiableList(typedList);
    }
  }

  /**
   * Gets the listeners count for the given event class.
   *
   * @param eventClass the event class
   * @return the listeners count
   */
  public int getCount(Class<? extends EventObject> eventClass) {
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(eventClass);
    return (list != null) ? list.size() : 0;
  }

  /**
   * Dispatches event with a custom filter that accepts both the listener and the event.
   *
   * @param <T> the generic type
   * @param event the event
   * @param filter a filter that accepts both the listener and the event
   */
  public <T extends EventObject> void dispatchEvent(T event,
      BiPredicate<EventListener<T>, T> filter) {
    // Get the list of listeners for the event class and execute the event
    CopyOnWriteArrayList<EventListener<?>> list = listeners.get(event.getClass());

    if (list != null && !list.isEmpty()) {
      for (EventListener<?> listener : list) {
        @SuppressWarnings("unchecked")
        EventListener<T> l = (EventListener<T>) listener;
        // Check if the filter allows this listener to receive the event
        if (filter.test(l, event)) {
          l.onEvent(event);
        }
      }
    }
  }

  /**
   * Dispatches event.
   *
   * @param <T> the generic type
   * @param event the event
   */
  public <T extends EventObject> void dispatchEvent(T event) {
    // Default behavior dispatches to all listeners
    dispatchEvent(event, (listener, e) -> true);
  }
}
