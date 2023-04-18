package org.dwcj.component.events.sinks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.BlurEvent;
import org.dwcj.component.events.EventDispatcher;

/**
 * This class is responsible for Blur event communication between the Java representation 
 * of the underlying BBj component.
 */
public class BlurEventSink extends AbstractSink {

  public BlurEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LOST_FOCUS);   
  }

  @Override
  public void handleEvent(BBjEvent ev) {
    BBjLostFocusEvent event = (BBjLostFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
//     map.put("temporary", event.isTemporary());
    map.put("client-validation-valid", event.isClientValidationValid());
    
    BlurEvent dwcEv = new BlurEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
