package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjAbstractTextKeypressEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.field.DwcMaskedField;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * An abstract class of a keypress event sink which would handle a BBjKeypressEvent and dispatch the
 * corresponding Java event.
 */
public class KeypressEventSink extends AbstractDwcEventSink {

  public KeypressEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher,
        (component instanceof DwcMaskedField) ? SysGuiEventConstants.ON_INPUT_KEYPRESS
            : SysGuiEventConstants.ON_EDIT_KEYPRESS);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    BBjAbstractTextKeypressEvent event = (BBjAbstractTextKeypressEvent) ev;

    map.put("keyCode", event.getKeyCode());
    map.put("altKey", event.isAltDown());
    map.put("cmdKey", event.isCmdDown());
    map.put("controlKey", event.isControlDown());
    map.put("shiftKey", event.isShiftDown());

    KeypressEvent dwcEv = new KeypressEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
