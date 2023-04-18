package org.dwcj.component.events.shared.sinks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.Sink;
import org.dwcj.component.events.shared.events.BlurEvent;

/**
 * Sink class responsible for communication between BBj and java.
 */
public class BlurEventSink extends Sink {

  public BlurEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LOST_FOCUS);   
  }

  @Override
  public void handleEvent(BBjEvent ev) {
    BBjLostFocusEvent event = (BBjLostFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
    map.put("temporary", event.isTemporary());
    map.put("client-validation-valid", event.isClientValidationValid());
    
    BlurEvent dwcEv = new BlurEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
