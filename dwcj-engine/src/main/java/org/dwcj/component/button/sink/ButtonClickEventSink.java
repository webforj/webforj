package org.dwcj.component.button.sink;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.events.EventDispatcher;
import org.dwcj.component.events.sinks.AbstractSink;

/**
 * Sink class responsible for communication between BBj and java.
 */
public final class ButtonClickEventSink extends AbstractSink {

  public ButtonClickEventSink(Button component, EventDispatcher dispatcher) {
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

    ButtonClickEvent dwcEv = new ButtonClickEvent((Button) this.component, map);
    this.dispatcher.dispatchEvent(dwcEv);
  }
}
