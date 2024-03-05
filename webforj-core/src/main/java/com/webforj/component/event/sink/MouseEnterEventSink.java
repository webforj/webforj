package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.component.event.MouseEnterEvent;
import java.util.HashMap;

/**
 * This class will map the BBjMouseEnterEvent event to a Java {@link MouseEnterEvent}.
 */
public class MouseEnterEventSink extends MouseEventSink {

  public MouseEnterEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_ENTER);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    MouseEnterEvent dwcEv = new MouseEnterEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }

}
