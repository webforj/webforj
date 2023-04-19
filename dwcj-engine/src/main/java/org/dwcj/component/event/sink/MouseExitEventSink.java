package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseExitEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;


/**
 * This class will map the BBjMouseExitEvent event to a Java {@link MouseEvent}.
 */
public class MouseExitEventSink extends AbstractMouseEventSink {

  public MouseExitEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_EXIT);
  }

  /**
   * Handles the BBj event and calls handleEventParams to map the 
   * event parameters and dispatch a new {@link MouseEvent}.
   *
   * @param ev A BBj mouse event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseExitEvent event = (BBjMouseExitEvent) ev;
    this.handleEventParams(event);
  }
}