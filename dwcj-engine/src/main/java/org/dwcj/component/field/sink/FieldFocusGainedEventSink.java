package org.dwcj.component.field.sink;

import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.FocusGainedEvent;
import org.dwcj.component.field.Field;

/**
 * Sink class responsible for communication between BBj and java.
 */
public final class FieldFocusGainedEventSink {

  private final Field field;
  private final EventDispatcher dispatcher;
  private BBjControl control = null;

  /**
   * Constructor for the sink class.
   *
   * @param field The Java field object
   * @param dispatcher The dispatcher for that object's events
   */
  public FieldFocusGainedEventSink(Field field, EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
    this.field = field;
    try {
      control = ComponentAccessor.getDefault().getBBjControl(field);
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
   * @param ev A BBj field push event
   */
  public void pushEvent(BBjGainedFocusEvent ev) { // NOSONAR
    FocusGainedEvent dwcEv = new FocusGainedEvent(this.field, new HashMap<>());
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
