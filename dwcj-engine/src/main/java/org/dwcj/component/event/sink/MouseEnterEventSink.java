package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEnterEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseEnterEvent;


/**
 * This class will map the BBjMouseEnterEvent event to a Java {@link MouseEnterEvent}.
 */
public class MouseEnterEventSink extends AbstractSink {

  public MouseEnterEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_OFF);
  }

  /**
   * Handles the BBj event and dispatches a new {@link MouseEnterEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    BBjMouseEnterEvent event = (BBjMouseEnterEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("legacyMouseButton", event.getLegacyMouseButton());
    map.put("mouseButton", event.getMouseButton());
    map.put("nativeMouseButton", event.getNativeMouseButton());
    map.put("originalControl", event.getOriginalControl());
    map.put("screenX", event.getScreenX());
    map.put("screenY", event.getScreenY());
    map.put("x", event.getX());
    map.put("y", event.getY());
    map.put("altDown", event.isAltDown());
    map.put("cmdDown", event.isCmdDown());
    map.put("controlDown", event.isControlDown());
    map.put("shiftDown", event.isShiftDown());


    MouseEnterEvent dwcEv = new MouseEnterEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
