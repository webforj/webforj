package org.dwcj.component.event.sink;


import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseEvent;

/**
 * Abstract class 
 * 
 */
public abstract class AbstractMouseEventSink extends AbstractSink {
  protected AbstractMouseEventSink(AbstractDwcComponent component, EventDispatcher dispatcher,
      int sysGuiConstant) {
    super(component, dispatcher, sysGuiConstant);
  }

  /**
   * Handles the BBj event and dispatches a new {@link MouseEvent}.
   *
   * @param ev A BBj event
   */
  public void handleEvent(BBjEvent ev){
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("mouseButton", event.getMouseButton());
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
