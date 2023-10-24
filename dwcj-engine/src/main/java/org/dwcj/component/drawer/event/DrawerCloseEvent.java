package org.dwcj.component.drawer.event;

import java.util.Map;
import org.dwcj.component.drawer.Drawer;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.webcomponent.annotation.EventExpressions;
import org.dwcj.component.webcomponent.annotation.EventName;

/**
 * Emitted when the drawer is opened.
 *
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-closed")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class DrawerCloseEvent extends ComponentEvent<Drawer> {

  /**
   * Creates a new event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerCloseEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
