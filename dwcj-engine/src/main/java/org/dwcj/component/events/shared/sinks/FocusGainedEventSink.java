package org.dwcj.component.events.shared.sinks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.Sink;
import org.dwcj.component.events.shared.events.FocusGainedEvent;


/**
 * Sink class responsible for communication between BBj and java.
 */
public final class FocusGainedEventSink extends Sink {

  public FocusGainedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_GAINED_FOCUS);
  }

  @Override
  public void handleEvent(BBjEvent ev) { // NOSONAR
    BBjGainedFocusEvent event = (BBjGainedFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
    map.put("temporary", event.isTemporary());
    
    FocusGainedEvent dwcEv = new FocusGainedEvent(this.component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
