package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.UncheckedEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjCheckOffEvent event to a Java {@link UncheckedEvent}.
 */
public class UncheckedEventSink extends AbstractSink {

  public UncheckedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_OFF);
  }

  /**
   * Handles the BBj event and dispatches a new {@link UncheckedEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    UncheckedEvent dwcEv = new UncheckedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
