package org.dwcj.component.window.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.sink.MouseEventSink;
import org.dwcj.component.window.Window;
import org.dwcj.component.window.event.WindowClickEvent;
import org.dwcj.dispatcher.EventDispatcher;

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
