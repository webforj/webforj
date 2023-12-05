package org.dwcj.component.drawer.event;

import java.util.Map;
import org.dwcj.component.drawer.Drawer;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.event.ComponentEvent;

/**
 * Emitted when the drawer is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "bbj-drawer-closed")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class DrawerCloseEvent extends ComponentEvent<Drawer> {

  /**
   * Creates a close Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerCloseEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
