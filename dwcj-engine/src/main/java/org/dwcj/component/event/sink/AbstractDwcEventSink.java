package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The AbstractDwcEventSink sink implements the required logic for setting and removing the callback
 * on a BBjControl. Subclasses must implement the handleEvent method which is responsible for
 * delegating the BBj event to the corresponding event listener to the Java component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class AbstractDwcEventSink implements DwcEventSink {
  private final DwcComponent<?> component;
  private EventDispatcher dispatcher;
  private final int eventType;
  private BBjControl control = null;
  private IDwcjBBjBridge dwcjHelper;

  /**
   * Constructor for the sink class.
   *
   * @param component The Java component
   * @param dispatcher The events dispatcher
   * @param eventType The type of the BBj event
   */
  protected AbstractDwcEventSink(DwcComponent<?> component, EventDispatcher dispatcher,
      int eventType) {
    this.component = component;
    this.dispatcher = dispatcher;
    this.eventType = eventType;

    if (Environment.getCurrent() != null) {
      setDwcjHelper(Environment.getCurrent().getDwcjHelper());
    }
  }

  /**
   * Set a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be set.
   */
  @Override
  public final void setCallback() {
    BBjControl theControl = getBbjControl();

    if (theControl != null) {
      // in tests the dwcjHelper is not set so we need to check for null
      dwcjHelper = getDwcjHelper();
      if (dwcjHelper == null) {
        return;
      }
      try {
        theControl.setCallback(eventType, getDwcjHelper().getEventProxy(this, "handleEvent"),
            "onEvent");
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjControl callback.", e);
      }
    }
  }

  /**
   * Removes a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be removed.
   */
  @Override
  public final void removeCallback() {
    BBjControl theControl = getBbjControl();

    if (theControl != null) {
      try {
        theControl.clearCallback(eventType);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to remove BBjControl callback.", e);
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
   * Sets the instance of the DwcjHelper.
   *
   * @param dwcjHelper The DwcjHelper instance.
   */
  void setDwcjHelper(IDwcjBBjBridge dwcjHelper) {
    this.dwcjHelper = dwcjHelper;
  }

  /**
   * Gets the instance of the DwcjHelper.
   *
   * @return The DwcjHelper instance.
   */
  IDwcjBBjBridge getDwcjHelper() {
    return this.dwcjHelper;
  }

  /**
   * Gets the Java component.
   *
   * @return The Java component.
   */
  protected final DwcComponent<?> getComponent() {
    return this.component;
  }

  /**
   * Gets the instance of the underlying BBjControl.
   *
   * @return The BBjControl instance.
   */
  private BBjControl getBbjControl() {
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
