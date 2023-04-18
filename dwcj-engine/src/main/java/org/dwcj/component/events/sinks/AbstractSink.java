package org.dwcj.component.events.sinks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Sink class responsible for communication between BBj and java.
 */
public abstract class AbstractSink {
  protected final AbstractDwcComponent component;
  protected final EventDispatcher dispatcher;
  private BBjControl control = null;
  private final int eventType;

  /**
   * Constructor for the sink class.
   *
   * @param component The Java component object
   * @param dispatcher The dispatcher for that object's events
   * @param eventType The type of the event
   */
  protected AbstractSink(AbstractDwcComponent component, EventDispatcher dispatcher, 
      int eventType) {
    this.component = component;
    this.dispatcher = dispatcher;
    this.eventType = eventType;

    try {
      control = ComponentAccessor.getDefault().getBBjControl(component);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to get instantiate the Sink.", e);
    }    
  }

  /**
   * Method is called to register an on-push event with the BBj object as needed when adding an
   * onClick event.
   */
  public void setCallback() {
    try {
      control.setCallback(eventType,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "handleEvent"), "onEvent");
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set callback.", e);
    }
  }

  /**
   * Method is called to deregister an on-push event with the BBj object as needed when removing an
   * onClick event.
   */
  public void removeCallback() {
    try {
      control.clearCallback(eventType);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to remove callback.", e);
    }
  }

  /**
   * Method responsible for calling the dispatcher event which calls the execute method on the
   * desired listener.
   *
   * @param ev A BBj event
   */
  public abstract void handleEvent(BBjEvent ev);

}