package org.dwcj.component.event;

import java.util.HashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import org.dwcj.App;
import org.dwcj.environment.namespace.NamespaceListener;
import org.dwcj.environment.namespace.event.NamespaceEvent;

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

  private HashMap<Class<? extends NamespaceEvent>, CopyOnWriteArrayList<NamespaceListener<?>>> namespaceListeners =
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
      list.remove(listener);
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

  public <T extends NamespaceEvent> void addNamespaceEventListener(Class<T> eventClass,
      NamespaceListener<T> listener) {
    CopyOnWriteArrayList<NamespaceListener<?>> list = namespaceListeners.get(eventClass);
    App.consoleLog("we are in the dispatcher for namespace");
    if (list == null) {
      list = new CopyOnWriteArrayList<>();
      namespaceListeners.put(eventClass, list);
    }

    list.add(listener);
  }

  public <T extends NamespaceEvent> void dispatchNamespaceEvent(T event) {
    CopyOnWriteArrayList<NamespaceListener<?>> list = namespaceListeners.get(event.getClass());

    if (list != null && !list.isEmpty()) {
      for (NamespaceListener<?> listener : list) {
        @SuppressWarnings("unchecked")
        NamespaceListener<T> l = (NamespaceListener<T>) listener;
        l.execute(event);
      }
    }
  }
}
