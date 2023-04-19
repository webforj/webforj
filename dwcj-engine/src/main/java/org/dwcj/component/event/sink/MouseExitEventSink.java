package org.dwcj.component.event.sink;

import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;


/**
 * This class will map the BBjMouseExitEvent event to a Java {@link MouseEvent}.
 */
public class MouseExitEventSink extends AbstractMouseEventSink {

  public MouseExitEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_MOUSE_EXIT);
  }
}