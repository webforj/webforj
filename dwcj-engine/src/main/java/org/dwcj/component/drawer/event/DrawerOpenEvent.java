package org.dwcj.component.drawer.event;

import java.util.Map;

import org.dwcj.component.drawer.Drawer;
import org.dwcj.component.webcomponent.annotations.EventExpressions;
import org.dwcj.component.webcomponent.annotations.EventName;
import org.dwcj.component.webcomponent.events.Event;

/**
 * Emitted when the drawer is opened.
 *
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-opened")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class DrawerOpenEvent extends Event<Drawer> {

  /**
   * Creates a new event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerOpenEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
