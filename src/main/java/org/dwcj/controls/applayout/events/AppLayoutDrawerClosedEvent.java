package org.dwcj.controls.applayout.events;

import java.util.Map;

import org.dwcj.controls.applayout.AppLayout;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Emitted when the drawer is closed.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-closed")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class AppLayoutDrawerClosedEvent extends Event<AppLayout> {

  /**
   * Creates a new event.
   * 
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppLayoutDrawerClosedEvent(AppLayout target, Map<String, Object> detail) {
    super(target, detail);
  }
}
