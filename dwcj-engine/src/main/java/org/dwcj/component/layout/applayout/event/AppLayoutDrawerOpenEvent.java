package org.dwcj.component.layout.applayout.event;

import java.util.Map;
import org.dwcj.component.event.Event;
import org.dwcj.component.layout.applayout.AppLayout;
import org.dwcj.component.webcomponent.annotation.EventExpressions;
import org.dwcj.component.webcomponent.annotation.EventName;

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
