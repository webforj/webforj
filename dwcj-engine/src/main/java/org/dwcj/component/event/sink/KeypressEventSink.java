package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEditKeypressEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.KeypressEvent;

/**
 * This class will map the BBjEditKeypressEvent to a Java {@link KeypressEvent}.
 */
public class KeypressEventSink extends AbstractSink {

  public KeypressEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_EDIT_KEYPRESS);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjEditKeypressEvent event = (BBjEditKeypressEvent) ev;
    Map<String, Object> map = new HashMap<>();

    map.put("keyCode", event.getKeyCode());
    map.put("keyCodeWithFlags", event.getKeyCodeWithFlags());
    map.put("modifiersEx", event.getModifiersEx());
    map.put("altDown", event.isAltDown());
    map.put("cmdDown", event.isCmdDown());
    map.put("controlDown", event.isControlDown());
    map.put("shiftDown", event.isShiftDown());

    KeypressEvent dwcEv = new KeypressEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }

}
