package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.BlurEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjLostFocusEvent event to a Java {@link BlurEvent}.
 */
public class BlurEventSink extends AbstractDwcEventSink {

  public BlurEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LOST_FOCUS);
  }

  /**
   * Handles the BBj event and dispatches a new {@link BlurEvent}.
   *
   * @param ev A BBj lost focus event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjLostFocusEvent event = (BBjLostFocusEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());
    map.put("client-validation-valid", event.isClientValidationValid());

    BlurEvent dwcEv = new BlurEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
