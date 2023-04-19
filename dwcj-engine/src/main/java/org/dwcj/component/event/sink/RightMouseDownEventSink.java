package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjRightMouseDownEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;


/**
 * This class will map the BBjRightMouseDownEvent event to a Java {@link MouseEvent}.
 */
public class RightMouseDownEventSink extends AbstractMouseEventSink {

  public RightMouseDownEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_RIGHT_MOUSE_DOWN);
  }

  /**
   * Handles the BBj event and calls handleEventParams to map the 
   * event parameters and dispatch a new {@link MouseEvent}.
   *
   * @param ev A BBj mouse event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjRightMouseDownEvent event = (BBjRightMouseDownEvent) ev;
    this.handleEventParams(event);
  }
}