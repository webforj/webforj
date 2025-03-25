package com.webforj.component.refresher.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.refresher.Refresher;
import java.util.Map;

/**
 * Emitted when the refresher is triggered.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@EventName(value = "dwc-refresh")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class RefresherRefreshEvent extends ComponentEvent<Refresher> {

  /**
   * Creates a refresher's refresh Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public RefresherRefreshEvent(Refresher target, Map<String, Object> detail) {
    super(target, detail);
  }
}
