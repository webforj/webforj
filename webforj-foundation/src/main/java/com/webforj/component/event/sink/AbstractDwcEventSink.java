package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.Component;
import com.webforj.component.DwcComponent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.UUID;

/**
 * The AbstractDwcEventSink sink implements the required logic for setting and removing the callback
 * on a BBjControl. Subclasses must implement the handleEvent method which is responsible for
 * delegating the BBj event to the corresponding event listener to the Java component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class AbstractDwcEventSink implements DwcEventSink<Component> {
  private final DwcComponent<?> component;
  private EventDispatcher dispatcher;
  private final Object eventType;
  private BBjControl control;
  private WebforjBBjBridge webforjHelper;

  /**
   * Constructor a new event sink for the given component and the BBj event type.
   *
   * @param component The Java component
   * @param dispatcher The events dispatcher
   * @param eventType The type of the BBj event
   * @param options The options to be passed to the BBj event callback if the event supports options
   */
  protected AbstractDwcEventSink(DwcComponent<?> component, EventDispatcher dispatcher,
      Object eventType) {
    this.component = component;
    this.dispatcher = dispatcher;
    this.eventType = eventType;

    if (Environment.getCurrent() != null) {
      setWebforjHelper(Environment.getCurrent().getBridge());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final String setCallback(Object options) {
    if (isConnected()) {
      try {
        CustomObject handler = getWebforjHelper().getEventProxy(this, "handleEvent");
        return doSetCallback(getControl(), options, handler, "onEvent");
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to set BBjControl callback.", e);
      }
    }

    return null;
  }

  /**
   * Removes a callback on an underlying BBj control.
   *
   * @throws WebforjRuntimeException if the callback cannot be removed.
   */
  @Override
  public final void removeCallback(String callbackId) {
    if (isConnected()) {
      try {
        doRemoveCallback(getControl(), callbackId);
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to remove BBjControl callback.", e);
      }
    }
  }

  /**
   * Handles the BBj event and delegates it to the corresponding event listener in the Java
   * component.
   *
   * @param ev A BBj event
   */
  public abstract void handleEvent(BBjEvent ev);

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean isConnected() {
    return getControl() != null && getWebforjHelper() != null;
  }

  /**
   * Gets the BBj event type.
   *
   * @return The BBj event type.
   */
  public final Object getEventType() {
    return this.eventType;
  }

  /**
   * Gets the Java component instance.
   *
   * @return The Java component.
   */
  @Override
  public final DwcComponent<?> getComponent() {
    return this.component;
  }

  /**
   * Sets the event dispatcher instance.
   *
   * @param dispatcher the event dispatcher instance.
   */
  void setEventDispatcher(EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }

  /**
   * Gets the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  @Override
  public final EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Sets the instance of the WebforjHelper.
   *
   * @param helper The WebforjHelper instance.
   */
  void setWebforjHelper(WebforjBBjBridge helper) {
    this.webforjHelper = helper;
  }

  /**
   * Gets the instance of the WebforjHelper.
   *
   * @return The WebforjHelper instance.
   */
  WebforjBBjBridge getWebforjHelper() {
    return this.webforjHelper;
  }

  /**
   * Do set a callback on the underlying BBj control.
   *
   * @param control The control
   * @param options The options object
   * @param handler The BBj CustomObject instance
   * @param callback The callback method name as defined in the handler
   *
   * @return the callback id.
   *
   * @throws BBjException if the callback cannot be set.
   */
  protected String doSetCallback(BBjControl control, Object options, CustomObject handler,
      String callback) throws BBjException {
    if (control != null) {
      control.setCallback(Integer.valueOf(String.valueOf(eventType)), handler, callback);
      return UUID.randomUUID().toString();
    }

    return null;
  }

  /**
   * Do remove a callback from underlying BBj control.
   *
   * @param control The control
   * @param callbackId The callback id.
   *
   * @throws BBjException if the callback cannot be removed.
   */
  protected void doRemoveCallback(BBjControl control, String callbackId) throws BBjException {
    if (control != null) {
      control.clearCallback(Integer.valueOf(String.valueOf(eventType)));
    }
  }

  /**
   * Gets the instance of the underlying BBjControl.
   *
   * @return The BBjControl instance.
   */
  protected BBjControl getControl() {
    try {
      if (control != null && !control.isDestroyed()) {
        return control;
      }

      control = ComponentAccessor.getDefault().getControl(component);
    } catch (BBjException | IllegalAccessException e) {
      // pass
    }

    return control;
  }
}
