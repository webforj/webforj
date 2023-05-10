package org.dwcj.component.event;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.sink.AbstractSink;
import org.dwcj.exceptions.DwcjRuntimeException;

/** Manages Events for a Control. */
public class EventManager<C extends AbstractDwcComponent> {

  private final EventDispatcher dispatcher = new EventDispatcher();

  private final C control;

  private HashMap<Class<? extends Event<?>>, EventController<? extends Event<?>>> events =
      new HashMap<>();

  public EventManager(C control) {
    this.control = control;
  }

  /**
   * Adds a Event.
   *
   * @param <E> the Eventtype
   * @param sink the Events sink
   * @param event the Event
   */
  public <E extends Event<?>> void addEvent(Class<? extends AbstractSink> sink, Class<E> event) {
    try {
      Constructor<?> constructor = sink.getConstructors()[0];

      EventController<E> eventController =
          new EventController<>(sink.cast(constructor.newInstance(control, dispatcher)), event);
      this.events.put(event, eventController);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to add Event.", e);
    }
  }

  /**
   * Adds a listener.
   *
   * @param <T> type of the Event
   * @param event the event class
   * @param eventListener the listener
   */
  public <T extends Event<?>> void addEventListener(Class<T> event,
      EventListener<T> eventListener) {
    @SuppressWarnings("unchecked")
    EventController<T> eventController = (EventController<T>) events.get(event);

    if (eventController == null) {
      return;
    }

    eventController.addEventListener(eventListener);
  }

  /**
   * Removes a listener.
   *
   * @param <T> type of the Event
   * @param event the event class
   * @param eventListener the listener
   */
  public <T extends Event<?>> void removeEventListener(Class<T> event,
      EventListener<T> eventListener) {
    @SuppressWarnings("unchecked")
    EventController<T> eventController = (EventController<T>) events.get(event);

    if (eventController == null) {
      return;
    }

    eventController.removeEventListener(eventListener);
  }

  /**
   * Calls the catchUp of the all the event controllers.
   */
  public void catchUp() {
    if (events.isEmpty()) {
      return;
    }

    for (EventController<?> eventController : events.values()) {
      eventController.catchUp();
    }
  }

  // Package private getter for testing
  HashMap<Class<? extends Event<?>>, EventController<? extends Event<?>>> getEventMap() {
    return this.events;
  }

  // Package private getter for testing
  EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

}
