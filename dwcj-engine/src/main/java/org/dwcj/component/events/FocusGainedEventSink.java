package org.dwcj.component.events;

import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.AbstractDwcComponent;

/**
 * Sink class responsible for communication between BBj and java.
 */
public final class FocusGainedEventSink {

  private final AbstractDwcComponent component;
  private final EventDispatcher dispatcher;
  private BBjControl control = null;

  /**
   * Constructor for the sink class.
   *
   * @param component The Java component object
   * @param dispatcher The dispatcher for that object's events
   */
  public FocusGainedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
    this.component = component;
    try {
      control = ComponentAccessor.getDefault().getBBjControl(component);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method is called to register an on-push event with the BBj object as needed when adding an
   * onClick event.
   */
  public void setCallback() {
    try {
      control.setCallback(SysGuiEventConstants.ON_GAINED_FOCUS,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method is called to deregister an on-push event with the BBj object as needed when removing an
   * onClick event.
   */
  public void removeCallback() {
    try {
      control.clearCallback(SysGuiEventConstants.ON_GAINED_FOCUS);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method responsible for calling the dispatcher event which calls the execute method on the
   * desired listener.
   *
   * @param ev A BBj component push event
   */
  public void pushEvent(BBjGainedFocusEvent ev) { // NOSONAR
    FocusGainedEvent dwcEv = new FocusGainedEvent(this.component, new HashMap<>());
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
