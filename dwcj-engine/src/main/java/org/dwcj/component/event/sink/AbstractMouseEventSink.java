package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjMouseEvent;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;

/**
 * An abstract class of a mouse event sink which would handle a BBjMouseEvent and dispatch the
 * corresponding Java event.
 */
public abstract class AbstractMouseEventSink extends AbstractEventSink {
  protected AbstractMouseEventSink(AbstractDwcComponent component, EventDispatcher dispatcher,
      int sysGuiConstant) {
    super(component, dispatcher, sysGuiConstant);
  }

  /**
   * Helper method to build the payload for BBjMouseEvents.
   *
   * @param ev the event
   * @return the payload
   */
  protected HashMap<String, Object> buildPayload(BBjMouseEvent ev) {
    HashMap<String, Object> map = new HashMap<>();

    map.put("mouseButton", ev.getMouseButton());
    map.put("screenX", ev.getScreenX());
    map.put("screenY", ev.getScreenY());
    map.put("x", ev.getX());
    map.put("y", ev.getY());
    map.put("altDown", ev.isAltDown());
    map.put("cmdDown", ev.isCmdDown());
    map.put("controlDown", ev.isControlDown());
    map.put("shiftDown", ev.isShiftDown());

    return map;
  }
}
