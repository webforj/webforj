package com.webforj.environment.namespace.sink;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.event.NamespaceEvent;
import com.webforj.environment.namespace.event.NamespaceKeyChangeEvent;

/**
 * {@code NamespaceKeyChangeEventSink} is an abstract class that implements the required logic for
 * setting and removing the "change" callback on the {@code BBjNamespace} for a specific key. It
 * will delegates the BBj event to the corresponding event listener to the Java component.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceKeyChangeEventSink extends NamespaceKeyEventSink {

  /**
   * Creates a new {@code NamespaceKeyChangeEventSink}.
   *
   * @param namespace The namespace
   * @param variableName The variable name
   * @param dispatcher The event dispatcher
   */
  public NamespaceKeyChangeEventSink(Namespace namespace, String variableName,
      EventDispatcher dispatcher) {
    super(namespace, variableName, dispatcher);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected NamespaceEvent createEvent(String key, Object oldValue, Object newValue) {
    return new NamespaceKeyChangeEvent(getComponent(), key, oldValue, newValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doSetCallback(BBjNamespace namespace, Object handler, String callback)
      throws BBjException {
    namespace.setCallbackForVariableChange(getKey(), handler, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doRemoveCallback(BBjNamespace namespace) throws BBjException {
    namespace.removeCallbackForVariableChange(getKey());
  }
}
