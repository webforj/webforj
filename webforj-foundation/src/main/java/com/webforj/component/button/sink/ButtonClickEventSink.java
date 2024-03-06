package com.webforj.component.button.sink;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.button.DwcButton;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjButtonPushEvent event to a {@link ButtonClickEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public final class ButtonClickEventSink extends AbstractDwcEventSink {

  public ButtonClickEventSink(DwcButton<?> component, EventDispatcher dispatcher) {
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

    ButtonClickEvent dwcEv = new ButtonClickEvent((DwcButton<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
