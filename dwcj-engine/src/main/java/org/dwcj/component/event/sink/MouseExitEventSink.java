package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseExitEvent;


/**
 * This class will map the BBjMouseExitEvent event to a Java {@link MouseExitEvent}.
 */
public class MouseExitEventSink extends AbstractMouseEventSink {

  public MouseExitEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_EXIT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    MouseExitEvent dwcEv = new MouseExitEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
