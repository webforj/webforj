package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Sink class responsible for communication between BBj and java.
 */
public abstract class AbstractSink {
  protected final AbstractDwcComponent component;
  protected final EventDispatcher dispatcher;
  private BBjControl control = null;
  private final int eventType;
  private IDwcjBBjBridge dwcjHelper;

  /**
   * Constructor for the sink class.
   *
   * @param component The Java component
   * @param dispatcher The dispatcher for that object's events
   * @param eventType The type of the BBj event
   */
  protected AbstractSink(AbstractDwcComponent component, EventDispatcher dispatcher,
      int eventType) {
    this.component = component;
    this.dispatcher = dispatcher;
    this.eventType = eventType;
    if (Environment.getInstance() != null) {
      setHelper(Environment.getInstance().getDwcjHelper());
    }
  }

  void setHelper(IDwcjBBjBridge dwcjHelper) {
    this.dwcjHelper = dwcjHelper;
  }

  // checks if the control is null and tries to create it
  protected boolean isControl() {
    if (this.control != null) {
      return true;
    }
    try {
      this.control = ComponentAccessor.getDefault().getBBjControl(component);
      return this.control != null;
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * Method to set a callback on an underlying BBj control.
   */
  public void setCallback() {
    if (!isControl()) {
      return;
    }

    try {
      this.control.setCallback(eventType, dwcjHelper.getEventProxy(this, "handleEvent"), "onEvent");
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set callback.", e);
    }
  }

  /**
   * Method to remove a callback on an underlying BBj control.
   */
  public void removeCallback() {
    if (!isControl()) {
      return;
    }

    try {
      control.clearCallback(eventType);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to remove callback.", e);
    }
  }

  public EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Method responsible for calling the dispatcher event which calls the execute method on the
   * desired listener.
   *
   * @param ev A BBj event
   */
  public abstract void handleEvent(BBjEvent ev);

}
