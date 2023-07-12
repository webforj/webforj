package org.dwcj.component.button.sink;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractEventSink;

/**
 * This class will map the BBjButtonPushEvent event to a {@link ButtonClickEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public final class ButtonClickEventSink extends AbstractEventSink {

  public ButtonClickEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_BUTTON_PUSH);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ButtonClickEvent}.
   *
   * @param ev A BBj button push event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjButtonPushEvent event = (BBjButtonPushEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("x", event.getX());
    map.put("y", event.getY());

    ButtonClickEvent dwcEv = new ButtonClickEvent(getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
