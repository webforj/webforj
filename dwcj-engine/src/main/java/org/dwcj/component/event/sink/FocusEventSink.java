package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.DwcComponent;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.component.event.FocusEvent;

/**
 * The class will map the BBjGainedFocusEvent to a Java {@link FocusEvent}.
 */
public class FocusEventSink extends AbstractDwcEventSink {

  public FocusEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_GAINED_FOCUS);
  }

  /**
   * Handles the BBj event and dispatches a new {@link FocusEvent}.
   *
   * @param ev A BBj gained focus event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjGainedFocusEvent event = (BBjGainedFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
    map.put("temporary", event.isTemporary());

    FocusEvent dwcEv = new FocusEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }

}
