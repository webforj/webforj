package org.dwcj.component.event.sink;

import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;


/**
 * This class will map the BBjRightMouseDownEvent event to a Java {@link MouseEvent}.
 */
public class RightMouseDownEventSink extends AbstractMouseEventSink {

  public RightMouseDownEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_RIGHT_MOUSE_DOWN);
  }
}