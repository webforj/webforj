package com.webforj.component.drawer.event;

import com.webforj.component.drawer.Drawer;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Emitted when the drawer is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "dwc-drawer-closed")
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
