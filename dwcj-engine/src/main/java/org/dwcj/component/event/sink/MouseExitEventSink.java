package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseExitEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseExitEvent;


/**
 * This class will map the BBjMouseExitEvent event to a Java {@link MouseExitEvent}.
 */
public class MouseExitEventSink extends AbstractSink {

  public MouseExitEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_OFF);
  }

  /**
   * Handles the BBj event and dispatches a new {@link MouseExitEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    BBjMouseExitEvent event = (BBjMouseExitEvent) ev;
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


    MouseExitEvent dwcEv = new MouseExitEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}