package com.webforj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.DwcComponent;
import com.webforj.component.event.ModifyEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjEditModifyEvent event to a Java {@link ModifyEvent}.
 */
public class ModifyEventSink extends AbstractDwcEventSink {

  public ModifyEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_EDIT_MODIFY);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjEditModifyEvent event = (BBjEditModifyEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("text", event.getText());

    ModifyEvent dwcEv = new ModifyEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }
}
