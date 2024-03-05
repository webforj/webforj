package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.UncheckEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjCheckOffEvent event to a Java {@link UncheckEvent}.
 */
public class UncheckEventSink extends AbstractDwcEventSink {

  public UncheckEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_OFF);
  }

  /**
   * Handles the BBj event and dispatches a new {@link UncheckEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    UncheckEvent dwcEv = new UncheckEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
