package org.dwcj.component.button.sink;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import java.util.HashMap;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.events.EventDispatcher;

/**
 * Sink class responsible for communication between BBj and java.
 */
public final class ButtonClickEventSink {

  private final Button button;
  private final EventDispatcher dispatcher;
  private BBjControl bbjControl = null;

  /**
   * Constructor for the sink class.
   *
   * @param button The Java button object
   * @param dispatcher The dispatcher for that object's events
   */
  public ButtonClickEventSink(Button button, EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
    this.button = button;
    try {
      bbjControl = ComponentAccessor.getDefault().getBBjControl(button);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method is called to register an on-push event with the BBj object 
   * as needed when adding an onClick event.
   */
  public void registerEvents() {
    try {
      bbjControl.setCallback(com.basis.bbj.proxyif.SysGuiEventConstants.ON_BUTTON_PUSH, // NOSONAR
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method is called to deregister an on-push event with the BBj object 
   * as needed when removing an onClick event.
   */
  public void deregisterEvents() {
    try {
      bbjControl.clearCallback(com.basis.bbj.proxyif.SysGuiEventConstants.ON_BUTTON_PUSH);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Method responsible for calling the dispatcher event which calls the execute method on the
   * desired listener.
   *
   * @param ev A BBj button push event
   */
  private void pushEvent(BBjButtonPushEvent ev) { // NOSONAR
    ButtonClickEvent dwcEv = new ButtonClickEvent(this.button, new HashMap<>());
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
