package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.event.CheckEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link CheckEvent}.
 */
public class CheckEventSink extends AbstractDwcEventSink {

  public CheckEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_ON);
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    CheckEvent dwcEv = new CheckEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
