package org.dwcj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjNavigatorMoveFirstEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.event.NavigatorMoveFirstEvent;
import org.dwcj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjNavigatorMoveFirstEvent event to a {@link NavigatorMoveFirstEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public final class NavigatorMoveFirstEventSink extends AbstractDwcEventSink {

  public NavigatorMoveFirstEventSink(Navigator component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_NAV_FIRST);
  }

  /**
   * Handles the BBj event and dispatches a new {@link NavigatorMoveFirstEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjNavigatorMoveFirstEvent event = (BBjNavigatorMoveFirstEvent) ev;

    NavigatorMoveFirstEvent dwcEv =
        new NavigatorMoveFirstEvent((Navigator) getComponent(), event.getEventMap());
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
