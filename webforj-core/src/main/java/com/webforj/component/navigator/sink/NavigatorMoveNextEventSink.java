package com.webforj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjNavigatorMoveNextEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.navigator.event.NavigatorMoveNextEvent;
import com.webforj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjNavigatorMoveNextEvent event to a {@link NavigatorMoveNextEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public final class NavigatorMoveNextEventSink extends AbstractDwcEventSink {

  public NavigatorMoveNextEventSink(Navigator component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_NAV_NEXT);
  }

  /**
   * Handles the BBj event and dispatches a new {@link NavigatorMoveNextEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjNavigatorMoveNextEvent event = (BBjNavigatorMoveNextEvent) ev;

    NavigatorMoveNextEvent dwcEv =
        new NavigatorMoveNextEvent((Navigator) getComponent(), event.getEventMap());
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
