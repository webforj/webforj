package org.dwcj.webcomponent.events;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * The EventDispatcher is minimalistic event manager that can be used to dispatch
 * events to listeners.
 * 
 * @author Hyyan Abo Fakher
 */
public class EventDispatcher {

  /**
   * The listeners.
   */
  private HashMap<Class<? extends Event<?>>, ArrayList<EventListener<?>>> listeners = new HashMap<>();

  /**
   * Adds an event listener.
   * 
   * @param <T>        the generic type
   * @param eventClass the event class
   * @param listener   the listener
   */
  public <T extends Event<?>> void addEventListener(Class<T> eventClass, EventListener<T> listener) {
    ArrayList<EventListener<?>> list = listeners.get(eventClass);

    if (list == null) {
      list = new ArrayList<>();
      listeners.put(eventClass, list);
    }

    list.add(listener);
  }

  /**
   * Removes an event listener.
   * 
   * @param <T>        the generic type
   * @param eventClass the event class
   * @param listener   the listener
   */
  public <T extends Event<?>> void removeEventListener(Class<T> eventClass, EventListener<T> listener) {
    ArrayList<EventListener<?>> list = listeners.get(eventClass);
    
    if (list != null) {
      list.remove(listener);
    }
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
   * @param <T>   the generic type
   * @param event the event
   */
  public <T extends Event<?>> void dispatchEvent(T event) {
    ArrayList<EventListener<?>> list = listeners.get(event.getClass());

    if (list != null) {
      for (EventListener<?> listener : list) {
        @SuppressWarnings("unchecked")
        EventListener<T> l = (EventListener<T>) listener;
        l.execute(event);
      }
    }
  }
}
