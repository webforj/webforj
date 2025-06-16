package com.webforj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjNamespaceEvent;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.event.sink.DwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.event.NamespaceEvent;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.UUID;

/**
 * {@code NamespaceEventSink} is an abstract class that implements the required logic for setting
 * and removing the callback on the {@code BBjNamespace}. It will delegates the BBj event to the
 * corresponding event listener to the Java component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.22
 */
public abstract class NamespaceEventSink implements DwcEventSink<Namespace> {
  private final Namespace namespace;
  private EventDispatcher dispatcher;

  /**
   * Constructs a new instance of {@link NamespaceEventSink}.
   *
   * @param namespace The associated namespace.
   * @param dispatcher The event dispatcher.
   */
  protected NamespaceEventSink(Namespace namespace, EventDispatcher dispatcher) {
    this.namespace = namespace;
    this.dispatcher = dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final String setCallback(Object options) {
    if (isConnected()) {
      try {
        WebforjBBjBridge bridge = getEnvironment().getBridge();
        CustomObject handler = bridge.getEventProxy(this, "handleEvent");
        doSetCallback(namespace.getBbjNamespace(), handler, "onEvent");
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to set BBjNamespace callback.", e);
      }
    }

    return UUID.randomUUID().toString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void removeCallback(String id) {
    if (isConnected()) {
      try {
        // check the event dispatcher has zero listeners left
        // before removing the callback
        if (getEventDispatcher().getCount(NamespaceEvent.class) == 0) {
          doRemoveCallback(namespace.getBbjNamespace());
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to remove BBjNamespace callback.", e);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean isConnected() {
    return this.namespace != null && Environment.isPresent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Namespace getComponent() {
    return this.namespace;
  }

  /**
   * Handles the BBj event and dispatches a new {@link NamespaceEvent}.
   *
   * @param event The BBj event to handle.
   */
  public void handleEvent(BBjNamespaceEvent event) {
    Object nv = event.getNewValue();
    Object ov = event.getOldValue();
    String variable = event.getVariableName();
    NamespaceEvent nsEv = createEvent(variable, ov, nv);
    getEventDispatcher().dispatchEvent(nsEv);
  }

  /**
   * Do set a callback on the underlying BBj namespace.
   *
   * @param BBjNamespace The BBj namespace
   * @param handler The BBj CustomObject instance
   * @param callback The callback method name as defined in the handler
   *
   * @throws BBjException if the callback cannot be set.
   */
  protected abstract void doSetCallback(BBjNamespace namespace, CustomObject handler,
      String callback) throws BBjException;

  /**
   * Do remove a callback from underlying BBj namespace.
   *
   * @param BBjNamespace The BBj namespace
   *
   * @throws BBjException if the callback cannot be removed.
   */
  protected abstract void doRemoveCallback(BBjNamespace namespace) throws BBjException;

  /**
   * Creates a new {@link NamespaceEvent} instance.
   *
   * @param key The key.
   * @param oldValue The old value.
   * @param newValue The new value.
   *
   * @return A new {@link NamespaceEvent} instance.
   */
  protected abstract NamespaceEvent createEvent(String key, Object oldValue, Object newValue);

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}
