package com.webforj.environment.namespace.sink;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.NamespaceEventSink;
import com.webforj.environment.namespace.event.NamespaceChangeEvent;
import com.webforj.environment.namespace.event.NamespaceEvent;

/**
 * {@code NamespaceChangeEventSink} is an abstract class that implements the required logic for
 * setting and removing the namespace "change" callback on the {@code BBjNamespace}. It will
 * delegates the BBj event to the corresponding event listener to the Java component.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceChangeEventSink extends NamespaceEventSink {

  /**
   * Creates a new {@code NamespaceChangeEventSink}.
   *
   * @param namespace The namespace
   * @param dispatcher The event dispatcher
   */
  public NamespaceChangeEventSink(Namespace namespace, EventDispatcher dispatcher) {
    super(namespace, dispatcher);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected NamespaceEvent createEvent(String variableName, Object oldValue, Object newValue) {
    return new NamespaceChangeEvent(getComponent(), variableName, oldValue, newValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doSetCallback(BBjNamespace namespace, Object handler, String callback)
      throws BBjException {
    namespace.setCallbackForNamespaceChange(handler, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doRemoveCallback(BBjNamespace namespace) throws BBjException {
    namespace.removeCallbackForNamespaceChange();
  }
}
