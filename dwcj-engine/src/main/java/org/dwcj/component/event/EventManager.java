package org.dwcj.component.event;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import org.dwcj.App;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.sink.AbstractSink;
import org.dwcj.exceptions.DwcjRuntimeException;

public class EventManager<T extends AbstractDwcComponent> {

  private final EventDispatcher dispatcher = new EventDispatcher();

  private final T control;

  private final HashMap<Class<? extends Event<?>>, EventController<?>> events = new HashMap<>();

  public EventManager(T control) {
    this.control = control;
  }

  /** TODO. */
  public <E extends Event<?>> void addEvent(Class<? extends AbstractSink> sink, Class<E> event) {
    try {
      Constructor<?> constructor = sink.getConstructors()[0];

      EventController<E> eventHandler =
          new EventController<>(sink.cast(constructor.newInstance(control, dispatcher)), event);
      this.events.put(event, eventHandler);
    } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
        | InvocationTargetException | SecurityException e) {
      App.consoleLog(e.toString());
      throw new DwcjRuntimeException("Failed to add Event.", e);
    }
  }

  public <T extends Event<?>> void addEventListener(Class<T> event,
      EventListener<T> eventListener) {
    App.consoleLog(events.toString());
    App.consoleLog(event.toString());

    App.consoleLog(events.get(event).toString());
    // App.consoleLog(((EventHandler<T>) events.get(event)).toString());

    EventController<T> eventHandler = (EventController<T>) events.get(event);

    eventHandler.addEventListener(eventListener);
  }

  public <T extends Event<?>> void removeEventListener(Class<T> event,
      EventListener<T> eventListener) {
    EventController<T> eventHandler = (EventController<T>) events.get(event);
    eventHandler.removeEventListener(eventListener);
  }

  public void catchUp() {
    if (events.isEmpty()) {
      return;
    }

    for (EventController<?> eventHandler : events.values()) {
      eventHandler.catchUp();
    }
  }

}
