package com.webforj.component.layout.applayout.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.applayout.AppLayout;
import java.util.Map;

/**
 * Emitted when the drawer is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "dwc-drawer-opened")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class AppLayoutDrawerOpenEvent extends ComponentEvent<AppLayout> {

  /**
   * Creates a open Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppLayoutDrawerOpenEvent(AppLayout target, Map<String, Object> detail) {
    super(target, detail);
  }
}
