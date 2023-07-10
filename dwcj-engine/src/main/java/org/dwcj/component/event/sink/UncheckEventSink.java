package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.UncheckEvent;

/**
 * This class will map the BBjCheckOffEvent event to a Java {@link UncheckEvent}.
 */
public class UncheckEventSink extends AbstractEventSink {

  public UncheckEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
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
