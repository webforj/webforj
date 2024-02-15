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
@EventName(value = "dwc-drawer-opened")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class DrawerOpenEvent extends ComponentEvent<Drawer> {

  /**
   * Creates an open Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public DrawerOpenEvent(Drawer target, Map<String, Object> detail) {
    super(target, detail);
  }
}
