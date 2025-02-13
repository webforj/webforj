package com.webforj.environment.namespace.sink;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.NamespaceEventSink;
import com.webforj.environment.namespace.event.NamespaceAccessEvent;
import com.webforj.environment.namespace.event.NamespaceEvent;

/**
 * {@code NamespaceAccessEventSink} is an abstract class that implements the required logic for
 * setting and removing the namespace "access" callback on the {@code BBjNamespace}. It will
 * delegates the BBj event to the corresponding event listener to the Java component.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceAccessEventSink extends NamespaceEventSink {

  /**
   * Creates a new {@code NamespaceAccessEventSink}.
   *
   * @param namespace The namespace
   * @param dispatcher The event dispatcher
   */
  public NamespaceAccessEventSink(Namespace namespace, EventDispatcher dispatcher) {
    super(namespace, dispatcher);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected NamespaceEvent createEvent(String variableName, Object oldValue, Object newValue) {
    return new NamespaceAccessEvent(getComponent(), variableName, oldValue, newValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doSetCallback(BBjNamespace namespace, CustomObject handler, String callback)
      throws BBjException {
    namespace.setCallbackForNamespace(handler, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doRemoveCallback(BBjNamespace namespace) throws BBjException {
    namespace.removeCallbackForNamespace();
  }
}
