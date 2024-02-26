package org.dwcj.component.layout.applayout.event;

import java.util.Map;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.layout.applayout.AppLayout;

/**
 * Emitted when the drawer is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "dwc-drawer-closed")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class AppLayoutDrawerCloseEvent extends ComponentEvent<AppLayout> {

  /**
   * Creates a close Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppLayoutDrawerCloseEvent(AppLayout target, Map<String, Object> detail) {
    super(target, detail);
  }
}
