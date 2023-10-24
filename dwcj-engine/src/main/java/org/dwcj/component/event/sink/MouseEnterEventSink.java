package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseEnterEvent;

/**
 * This class will map the BBjMouseEnterEvent event to a Java {@link MouseEnterEvent}.
 */
public class MouseEnterEventSink extends AbstractMouseEventSink {

  public MouseEnterEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_ENTER);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    MouseEnterEvent dwcEv = new MouseEnterEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }

}
