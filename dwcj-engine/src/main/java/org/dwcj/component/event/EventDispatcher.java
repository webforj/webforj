package org.dwcj.component.event;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

/**
 * The EventDispatcher is a minimalistic event manager that can be used to dispatch component events
 * to listeners.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class EventDispatcher {

  private final Map<Class<? extends ComponentEvent<?>>, CopyOnWriteArrayList<ComponentEventListener<?>>> listeners =
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
  public <T extends ComponentEvent<?>> ListenerRegistration<T> addListener(Class<T> eventClass,
      ComponentEventListener<T> listener) {
    CopyOnWriteArrayList<ComponentEventListener<?>> list =
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
  public <T extends ComponentEvent<?>> void removeListener(Class<T> eventClass,
      ComponentEventListener<T> listener) {
    CopyOnWriteArrayList<ComponentEventListener<?>> list =
        listeners.computeIfAbsent(eventClass, k -> new CopyOnWriteArrayList<>());
    list.remove(listener);
  }

  /**
   * Removes all event listeners of a given type.
   *
   * @param <T> the generic type
   * @param eventClass the event class for which listeners should be removed
   */
  public <T extends ComponentEvent<?>> void removeAllListeners(Class<T> eventClass) {
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
  public <T extends ComponentEvent<?>> boolean hasListener(Class<T> eventClass,
      ComponentEventListener<T> listener) {
    CopyOnWriteArrayList<ComponentEventListener<?>> list = listeners.get(eventClass);
    return list != null && list.contains(listener);
  }

  /**
   * Checks if there are any listeners registered for a specific event class.
   *
   * @param <T> the generic type
   * @param eventClass the event class
   * @return true if there are listeners registered, false otherwise
   */
  public <T extends ComponentEvent<?>> boolean hasListener(Class<T> eventClass) {
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
  public <T extends ComponentEvent<?>> List<ComponentEventListener<T>> getListeners(
      Class<T> eventClass) {
    CopyOnWriteArrayList<ComponentEventListener<?>> list = listeners.get(eventClass);

    @SuppressWarnings("unchecked")
    List<ComponentEventListener<T>> typedList = list.stream()
        .map(listener -> (ComponentEventListener<T>) listener).collect(Collectors.toList());
    return Collections.unmodifiableList(typedList);
  }

  /**
   * Gets the listeners count for the given event class.
   *
   * @param eventClass the event class
   * @return the listeners count
   */
  public int getCount(Class<? extends ComponentEvent<?>> eventClass) {
    CopyOnWriteArrayList<ComponentEventListener<?>> list = listeners.get(eventClass);
    return (list != null) ? list.size() : 0;
  }

  /**
   * Dispatches event.
   *
   * @param <T> the generic type
   * @param event the event
   */
  public <T extends ComponentEvent<?>> void dispatchEvent(T event) {
    // Get the list of listeners for the event class and execute the event
    CopyOnWriteArrayList<ComponentEventListener<?>> list = listeners.get(event.getClass());

    if (list != null && !list.isEmpty()) {
      for (ComponentEventListener<?> listener : list) {
        @SuppressWarnings("unchecked")
        ComponentEventListener<T> l = (ComponentEventListener<T>) listener;
        l.onComponentEvent(event);
      }
    }
  }
}
