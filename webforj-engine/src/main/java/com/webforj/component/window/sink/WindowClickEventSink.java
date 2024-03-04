package com.webforj.component.window.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.MouseEventSink;
import com.webforj.component.window.Window;
import com.webforj.component.window.event.WindowClickEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the {@code BBjClickEvent} event to a Java {@link WindowClickEventSink}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class WindowClickEventSink extends MouseEventSink {

  public WindowClickEventSink(Window component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CLICK);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);
    map.put("clickCount", event.getClickCount());

    WindowClickEvent dwcEv = new WindowClickEvent((Window) getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
