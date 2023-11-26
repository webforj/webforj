package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.DwcComponent;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.component.event.ModifyEvent;

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
