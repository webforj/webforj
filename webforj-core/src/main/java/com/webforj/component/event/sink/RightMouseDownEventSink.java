package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.RightMouseDownEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjRightMouseDownEvent event to a Java {@link MouseEvent}.
 */
public class RightMouseDownEventSink extends MouseEventSink {

  public RightMouseDownEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_RIGHT_MOUSE_DOWN);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    RightMouseDownEvent dwcEv = new RightMouseDownEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }

}
