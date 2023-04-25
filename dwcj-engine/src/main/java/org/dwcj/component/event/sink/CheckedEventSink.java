package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.CheckedEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link CheckedEvent}.
 */
public class CheckedEventSink extends AbstractSink {

  public CheckedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_ON);
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckedEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    CheckedEvent dwcEv = new CheckedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
