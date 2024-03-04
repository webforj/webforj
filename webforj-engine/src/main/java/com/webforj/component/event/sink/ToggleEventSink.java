package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjCheckChangeEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.ToggleEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.DwcjRuntimeException;
import java.util.HashMap;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link ToggleEvent}.
 */
public class ToggleEventSink extends AbstractDwcEventSink {

  public ToggleEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_CHANGE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ToggleEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    try {
      BBjCheckChangeEvent event = (BBjCheckChangeEvent) ev;
      HashMap<String, Object> map = new HashMap<>();
      map.put("toggled", event.isChecked());
      ToggleEvent dwcEv = new ToggleEvent(this.getComponent(), map);
      this.getEventDispatcher().dispatchEvent(dwcEv);
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
