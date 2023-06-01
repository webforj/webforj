package org.dwcj.component.htmledit.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjStateChangeEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractSink;
import org.dwcj.component.htmledit.event.StateChangeEvent;

/**
 * This class will map the BBjStateChange event to a Java {@link StateChangeEvent}.
 */
public class StateChangeEventSink extends AbstractSink {

  public StateChangeEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_STATE_CHANGE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link StateChangeEvent}.
   *
   * @param ev A BBj page loaded event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjStateChangeEvent event = (BBjStateChangeEvent) ev;
    HashMap<String, Object> map = new HashMap<>();


    map.put("state", event.getState());
    map.put("value", event.getValue());

    StateChangeEvent dwcEv = new StateChangeEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
