package com.webforj.environment.namespace.sink;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.event.NamespaceEvent;
import com.webforj.environment.namespace.event.NamespaceKeyAccessEvent;

/**
 * {@code NamespaceKeyAccessEventSink} is an abstract class that implements the required logic for
 * setting and removing the "access" callback on the {@code BBjNamespace} for a specific key. It
 * will delegates the BBj event to the corresponding event listener to the Java component.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceKeyAccessEventSink extends NamespaceKeyEventSink {

  /**
   * Creates a new {@code NamespaceKeyAccessEventSink}.
   *
   * @param namespace The namespace
   * @param variableName The variable name
   * @param dispatcher The event dispatcher
   */
  public NamespaceKeyAccessEventSink(Namespace namespace, String variableName,
      EventDispatcher dispatcher) {
    super(namespace, variableName, dispatcher);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected NamespaceEvent createEvent(String key, Object oldValue, Object newValue) {
    return new NamespaceKeyAccessEvent(getComponent(), key, oldValue, newValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doSetCallback(BBjNamespace namespace, CustomObject handler, String callback)
      throws BBjException {
    namespace.setCallbackForVariable(getKey(), handler, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doRemoveCallback(BBjNamespace namespace) throws BBjException {
    namespace.removeCallbackForVariable(getKey());
  }
}
