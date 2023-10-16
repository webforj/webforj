package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.RightMouseDownEvent;

/**
 * This class will map the BBjRightMouseDownEvent event to a Java {@link MouseEvent}.
 */
public class RightMouseDownEventSink extends AbstractMouseEventSink {

  public RightMouseDownEventSink(DwcComponent<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_RIGHT_MOUSE_DOWN);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjMouseEvent event = (BBjMouseEvent) ev;
    HashMap<String, Object> map = super.buildPayload(event);

    RightMouseDownEvent dwcEv = new RightMouseDownEvent(this.getComponent(), map);
    this.getEventDispatcher().dispatchEvent(dwcEv);
  }

}
