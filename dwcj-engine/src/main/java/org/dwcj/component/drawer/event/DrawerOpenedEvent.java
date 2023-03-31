package org.dwcj.component.drawer.event;

import java.util.Map;

import org.dwcj.component.drawer.Drawer;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Emitted when the drawer is opened.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-opened")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class DrawerOpenedEvent extends Event<Drawer> {

  /**
   * Creates a new event.
   * 
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerOpenedEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
