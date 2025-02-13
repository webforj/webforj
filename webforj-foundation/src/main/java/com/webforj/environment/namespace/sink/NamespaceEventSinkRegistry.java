package com.webforj.environment.namespace.sink;

import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.NamespaceEventSink;
import com.webforj.environment.namespace.event.NamespaceEvent;

/**
 * {@code NamespaceEventSinkRegistry} is used to manage the event listeners (add/remove) for a
 * namespace sink and the corresponding event.
 *
 * @author Hyyan Abo Fakher
 * @since 24.22
 */
public class NamespaceEventSinkRegistry<E extends NamespaceEvent>
    extends EventSinkListenerRegistry<E, Namespace> {

  /**
   * Creates a new {@code NamespaceEventSinkRegistry}.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public NamespaceEventSinkRegistry(NamespaceEventSink sink, Class<? super E> event) {
    super(sink, event);
  }
}
