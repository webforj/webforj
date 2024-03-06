package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.MouseExitEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;


/**
 * This class will map the BBjMouseExitEvent event to a Java {@link MouseExitEvent}.
 */
public class MouseExitEventSink extends MouseEventSink {

  public MouseExitEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_EXIT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    MouseExitEvent dwcEv = new MouseExitEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
