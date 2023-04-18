package org.dwcj.component.applayout.event;

import java.util.Map;
import org.dwcj.component.applayout.AppLayout;
import org.dwcj.component.event.Event;
import org.dwcj.component.webcomponent.annotations.EventExpressions;
import org.dwcj.component.webcomponent.annotations.EventName;

/**
 * Emitted when the drawer is closed.
 *
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-closed")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class AppLayoutDrawerCloseEvent extends Event<AppLayout> {

  /**
   * Creates a new event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppLayoutDrawerCloseEvent(AppLayout target, Map<String, Object> detail) {
    super(target, detail);
  }
}
