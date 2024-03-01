package org.dwcj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjNavigatorMoveLastEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.event.NavigatorMoveLastEvent;
import org.dwcj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjNavigatorMoveLastEvent event to a {@link NavigatorMoveLastEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public final class NavigatorMoveLastEventSink extends AbstractDwcEventSink {

  public NavigatorMoveLastEventSink(Navigator component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_NAV_LAST);
  }

  /**
   * Handles the BBj event and dispatches a new {@link NavigatorMoveLastEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjNavigatorMoveLastEvent event = (BBjNavigatorMoveLastEvent) ev;

    NavigatorMoveLastEvent dwcEv =
        new NavigatorMoveLastEvent((Navigator) getComponent(), event.getEventMap());
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
