package org.dwcj.component.event;

import org.dwcj.component.event.sink.DwcEventSink;

/**
 * EventSinkListenerRegistry is used to manage the event listeners (add/remove) for a Control sink
 * and the corresponding event.
 *
 * @param <T> the event type
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public class EventSinkListenerRegistry<T extends ComponentEvent<?>> {
  private final DwcEventSink sink;
  private final Class<T> event;

  /**
   * Creates a new EventSinkManager.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public EventSinkListenerRegistry(DwcEventSink sink, Class<T> event) {
    this.sink = sink;
    this.event = event;
  }

  /**
   * Adds an event listener.
   *
   * @param listener The event listener to be added
   */
  public ListenerRegistration<T> addEventListener(ComponentEventListener<T> listener) {
    EventDispatcher dispatcher = getEventDispatcher();
    ListenerRegistration<T> registration = getEventDispatcher().addListener(event, listener);

    // we should only invoke the sink when the listener count is 1
    if (dispatcher.getCount(event) == 1) {
      getSink().setCallback();
    }

    return new DwcListenerRegistration(registration);
  }

  /**
   * Catches up the sink with the current state of the event dispatcher.
   */
  public void attach() {
    if (getEventDispatcher().getCount(event) > 0) {
      getSink().setCallback();
    }
  }

  /**
   * Gets the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  public EventDispatcher getEventDispatcher() {
    return getSink().getEventDispatcher();
  }

  /**
   * Gets the sink instance.
   *
   * @return the sink instance.
   */
  DwcEventSink getSink() {
    return sink;
  }

  /**
   * A custom DWC listener registration that will remove the callback from the sink when there are
   * no more listeners registered for the event.
   *
   * @author Hyyan Abo Fakher
   */
  private class DwcListenerRegistration extends ListenerRegistration<T> {

    DwcListenerRegistration(ListenerRegistration<T> registration) {
      super(registration.getEventDispatcher(), registration.getEventClass(),
          registration.getListener());
    }

    @Override
    public void remove() {
      EventDispatcher dispatcher = getSink().getEventDispatcher();
      dispatcher.removeListener(getEventClass(), getListener());

      if (dispatcher.getCount(getEventClass()) == 0) {
        sink.removeCallback();
      }
    }
  }
}
