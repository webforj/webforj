package org.dwcj.component.events.sinks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.FocusEvent;

/**
 * This class is responsible for Focus event communication between the Java representation 
 * of the underlying BBj component.
 */
public class FocusEventSink extends AbstractSink {

  public FocusEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_GAINED_FOCUS);
  }

  @Override
  public void handleEvent(BBjEvent ev) {
    BBjGainedFocusEvent event = (BBjGainedFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
    map.put("temporary", event.isTemporary());

    FocusEvent dwcEv = new FocusEvent(this.component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
    
}
