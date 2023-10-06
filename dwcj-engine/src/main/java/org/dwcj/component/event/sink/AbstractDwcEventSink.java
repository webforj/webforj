package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.AbstractDwcComponent;
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
  private final AbstractDwcComponent component;
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
  protected AbstractDwcEventSink(AbstractDwcComponent component, EventDispatcher dispatcher,
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
  public void setCallback() {
    BBjControl theControl = getBBjControl();

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
   * Remove a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be removed.
   */
  @Override
  public void removeCallback() {
    BBjControl theControl = getBBjControl();

    if (theControl != null) {
      try {
        theControl.clearCallback(eventType);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to remove BBjControl callback.", e);
      }
    }
  }

  /**
   * Handle the BBj event and delegate it to the corresponding event listener to the Java component.
   *
   * @param ev A BBj event
   */
  public abstract void handleEvent(BBjEvent ev);

  /**
   * Set the event dispatcher instance.
   *
   * @param dispatcher the event dispatcher instance.
   */
  void setEventDispatcher(EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }

  /**
   * Get the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  @Override
  public EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Set the instance of the DwcjHelper.
   *
   * @param dwcjHelper The DwcjHelper instance.
   */
  void setDwcjHelper(IDwcjBBjBridge dwcjHelper) {
    this.dwcjHelper = dwcjHelper;
  }

  /**
   * Get the instance of the DwcjHelper.
   *
   * @return The DwcjHelper instance.
   */
  IDwcjBBjBridge getDwcjHelper() {
    return this.dwcjHelper;
  }

  /**
   * Get the Java component.
   *
   * @return The Java component.
   */
  protected AbstractDwcComponent getComponent() {
    return this.component;
  }

  /**
   * Get the instance of the underlying BBjControl.
   *
   * @return The BBjControl instance.
   */
  private BBjControl getBBjControl() {
    if (this.control != null) {
      return this.control;
    }

    try {
      this.control = ComponentAccessor.getDefault().getBBjControl(component);
    } catch (IllegalAccessException e) {
      // pass
    }

    return control;
  }
}
