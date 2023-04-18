package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.CheckOnEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link CheckOnEvent}.
 */
public class CheckOnEventSink extends AbstractSink {

  public CheckOnEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_OFF);
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckOnEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    CheckOnEvent dwcEv = new CheckOnEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
