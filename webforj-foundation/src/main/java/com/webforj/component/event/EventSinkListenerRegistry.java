package com.webforj.component.event;

import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.event.sink.DwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
  private final Class<? super T> event;
  private final List<DwcListenerRegistration> registrations = new ArrayList<>();
  private String singleCallbackId = "";
  private final Map<EventListener<T>, String> callbackIds = new HashMap<>();

  /**
   * Creates a new EventSinkManager.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public EventSinkListenerRegistry(DwcEventSink sink, Class<? super T> event) {
    this.sink = sink;
    this.event = event;
  }

  /**
   * Adds an event listener.
   *
   * @param listener The event listener to be added
   * @param options The options to be passed to the BBj event callback if the event supports options
   */
  public ListenerRegistration<T> addEventListener(EventListener<T> listener, Object options) {
    ListenerRegistration<T> registration = getEventDispatcher().addListener(event, listener);
    DwcListenerRegistration dwcRegistration = new DwcListenerRegistration(registration, options);

    if (getSink().isConnected()) {
      dwcRegistration.connect();
    } else {
      registrations.add(dwcRegistration);
    }

    return dwcRegistration;
  }

  /**
   * Adds an event listener.
   *
   * @param listener The event listener to be added
   */
  public ListenerRegistration<T> addEventListener(EventListener<T> listener) {
    return addEventListener(listener, null);
  }

  /**
   * Catches up the sink with the current state of the event dispatcher.
   */
  public void attach() {
    registrations.forEach(DwcListenerRegistration::connect);
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
   * Gets the callback id for the given listener.
   *
   * @param listener The listener to get the callback id for.
   *
   * @return the callback id for the given listener if the registry is connected
   */
  public String getCallbackId(EventListener<T> listener) {
    if (!singleCallbackId.equals("")) {
      return singleCallbackId;
    }

    return callbackIds.get(listener);
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
  private final class DwcListenerRegistration extends ListenerRegistration<T> {
    private final Object options;

    DwcListenerRegistration(ListenerRegistration<T> registration, Object options) {
      super(getEventDispatcher(), registration.getEventClass(), registration.getListener());
      this.options = options;
    }

    @Override
    public void remove() {
      EventDispatcher dispatcher = getEventDispatcher();
      dispatcher.removeListener(getEventClass(), getListener());
      DwcEventSink eventSink = getSink();

      if (!eventSink.isConnected()) {
        return;
      }

      String id = getCallbackId();
      if (eventSink.isMultipleCallbacks()) {
        eventSink.removeCallback(id);
      } else {
        // we should only invoke the sink when the listener count is 0
        if (getEventDispatcher().getCount(getEventClass()) == 0) {
          eventSink.removeCallback(id);
          singleCallbackId = "";
        }
      }
    }

    String getCallbackId() {
      return EventSinkListenerRegistry.this.getCallbackId(getListener());
    }

    void connect() {
      DwcEventSink eventSink = getSink();

      if (eventSink.isMultipleCallbacks()) {
        String callbackId = eventSink.setCallback(options);
        callbackIds.put(getListener(), callbackId);
      } else {
        if (singleCallbackId.equals("")) {
          singleCallbackId = eventSink.setCallback(options);
          callbackIds.put(getListener(), singleCallbackId);
          eventSink.getComponent().addLifecycleObserver((component, lifecycleEvent) -> {
            if (lifecycleEvent == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
              callbackIds.remove(getListener());
              singleCallbackId = "";
            }
          });
        }
      }
    }
  }
}
