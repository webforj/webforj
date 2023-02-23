package org.dwcj.controls.drawer.events;

import java.util.Map;

import org.dwcj.controls.drawer.Drawer;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Emitted when the drawer is opened.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-closed")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class DrawerClosedEvent extends Event<Drawer> {

  /**
   * Creates a new event.
   * 
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerClosedEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
