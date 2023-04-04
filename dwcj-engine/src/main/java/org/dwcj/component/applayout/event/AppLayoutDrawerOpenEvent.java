package org.dwcj.component.applayout.event;

import java.util.Map;

import org.dwcj.component.applayout.AppLayout;
import org.dwcj.component.webcomponent.annotations.EventExpressions;
import org.dwcj.component.webcomponent.annotations.EventName;
import org.dwcj.component.webcomponent.events.Event;

/**
 * Emitted when the drawer is closed.
 *
 * @author Hyyan Abo Fakher
 */
@EventName(value = "bbj-drawer-opened")
@EventExpressions(filter = "event.target.isSameNode(component)")
public final class AppLayoutDrawerOpenEvent extends Event<AppLayout> {

  /**
   * Creates a new event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppLayoutDrawerOpenEvent(AppLayout target, Map<String, Object> detail) {
    super(target, detail);
  }
}
