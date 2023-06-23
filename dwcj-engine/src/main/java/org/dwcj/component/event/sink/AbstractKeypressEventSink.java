package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjAbstractTextKeypressEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;

/**
 * An abstract class of a keypress event sink which would handle a BBjKeypressEvent and dispatch the
 * corresponding Java event.
 */
public abstract class AbstractKeypressEventSink extends AbstractEventSink {

  protected AbstractKeypressEventSink(AbstractDwcComponent component, EventDispatcher dispatcher,
      int sysGuiConstant) {
    super(component, dispatcher, sysGuiConstant);
  }

  /**
   * Helper method to build the payload for BBjKeyPressEvents.
   *
   * @param ev the event
   * @return the payload
   */
  protected HashMap<String, Object> buildPayload(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    BBjAbstractTextKeypressEvent event = (BBjAbstractTextKeypressEvent) ev;

    map.put("keyCode", event.getKeyCode());
    map.put("altKey", event.isAltDown());
    map.put("cmdKey", event.isCmdDown());
    map.put("controlKey", event.isControlDown());
    map.put("shiftKey", event.isShiftDown());

    return map;
  }

}
