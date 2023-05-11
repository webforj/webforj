package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjCheckChangeEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.CheckChangedEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link CheckChangeEvent}.
 */
public class CheckChangedEventSink extends AbstractSink {

  public CheckChangedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_CHANGE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckChangedEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    try {
      BBjCheckChangeEvent event = (BBjCheckChangeEvent) ev;
      HashMap<String, Object> map = new HashMap<>();
      map.put("checked", event.isChecked());
      CheckChangedEvent dwcEv = new CheckChangedEvent(component, map);
      this.dispatcher.dispatchEvent(dwcEv);
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
