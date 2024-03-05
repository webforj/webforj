package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.component.Component;
import com.webforj.component.DwcComponent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * An abstract class of a mouse event sink which would handle a BBjMouseEvent and dispatch the
 * corresponding Java event.
 */
public abstract class MouseEventSink extends AbstractDwcEventSink {
  protected MouseEventSink(DwcComponent<?> component, EventDispatcher dispatcher,
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

    BBjControl originalControl = ev.getOriginalControl();
    Object userData = null;
    Component component = null;

    // Set default value
    map.put("originalComponent", null);

    try {
      userData = originalControl.getUserData();
      if (userData instanceof Component) {
        component = (Component) userData;
        map.put("originalComponent", component);
      }
    } catch (Exception e) {
      // Ignore
    }

    return map;
  }
}
