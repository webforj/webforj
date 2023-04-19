package org.dwcj.component.event.sink;


import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseEvent;

/**
 * 
 */
public abstract class AbstractMouseEventSink extends AbstractSink {
  protected AbstractMouseEventSink(AbstractDwcComponent component, EventDispatcher dispatcher,
      int sysGuiConstant) {
    super(component, dispatcher, sysGuiConstant);
  }

  /**
   * Supposed to be overwritten with a specific implementation casting ev
   * into a BBjMouseEvent and then calling handleEventParams with it.
   *
   * @param ev A BBj mouse event
   */
  public abstract void handleEvent(BBjEvent ev);
  
  /**
   * Handles the BBj mouse event and dispatches a new {@link MouseEvent}.
   *
   * @param event A BBj mouse event
   */
  protected void handleEventParams(BBjMouseEvent event) {
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


    MouseEvent dwcEv = new MouseEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
