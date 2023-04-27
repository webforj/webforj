package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.ComponentScrollEvent;
import org.dwcj.component.event.EventDispatcher;

/**
 * This class will map the BBjControlScrollEvent to a Java {@link ComponentScrollEvent}.
 */
public class ComponentScrollSink extends AbstractSink {

  public ComponentScrollSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CONTROL_SCROLL);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjControlScrollEvent event = (BBjControlScrollEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("orientation", event.getOrientation());
    map.put("position", event.getPosition());
    map.put("adjusting", event.isAdjusting());

    ComponentScrollEvent dwcEv = new ComponentScrollEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
