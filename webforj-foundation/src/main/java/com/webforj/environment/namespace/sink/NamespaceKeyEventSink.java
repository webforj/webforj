package com.webforj.environment.namespace.sink;

import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.NamespaceEventSink;

abstract class NamespaceKeyEventSink extends NamespaceEventSink {
  private final String key;

  /**
   * Creates a new {@code NamespaceKeyEventSink}.
   *
   * @param namespace The namespace
   * @param key The key
   * @param dispatcher The event dispatcher
   */
  protected NamespaceKeyEventSink(Namespace namespace, String key, EventDispatcher dispatcher) {
    super(namespace, dispatcher);
    this.key = key;
  }

  /**
   * Get the key which is contained in the namespace event.
   *
   * @return the key
   */
  protected String getKey() {
    return key;
  }
}
