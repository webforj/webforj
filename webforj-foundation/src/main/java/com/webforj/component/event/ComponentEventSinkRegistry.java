package com.webforj.component.event;

import com.webforj.component.Component;
import com.webforj.component.event.sink.DwcEventSink;

/**
 * ComponentSinkRegistry is used to manage the event listeners (add/remove) for a control sink and
 * the corresponding event.
 *
 * @param <E> the event type
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class ComponentEventSinkRegistry<E extends ComponentEvent<?>>
    extends EventSinkListenerRegistry<E, Component> {

  /**
   * Creates a new ComponentEventSinkListenerRegistry.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public ComponentEventSinkRegistry(DwcEventSink<Component> sink, Class<? super E> event) {
    super(sink, event);
  }
}
