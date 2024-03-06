package com.webforj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjNavigatorMovePreviousEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.navigator.event.NavigatorMovePreviousEvent;
import com.webforj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjNavigatorMovePreviousEvent event to a
 * {@link NavigatorMovePreviousEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public final class NavigatorMovePreviousEventSink extends AbstractDwcEventSink {

  public NavigatorMovePreviousEventSink(Navigator component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_NAV_PREVIOUS);
  }

  /**
   * Handles the BBj event and dispatches a new {@link NavigatorMovePreviousEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjNavigatorMovePreviousEvent event = (BBjNavigatorMovePreviousEvent) ev;

    NavigatorMovePreviousEvent dwcEv =
        new NavigatorMovePreviousEvent((Navigator) getComponent(), event.getEventMap());
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
