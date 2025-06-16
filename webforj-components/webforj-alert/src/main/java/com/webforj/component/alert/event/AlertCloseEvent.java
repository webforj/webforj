package com.webforj.component.alert.event;


import com.webforj.component.alert.Alert;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Emitted when the alert is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@EventName(value = "dwc-closed")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class AlertCloseEvent extends ComponentEvent<Alert> {

  /**
   * Creates a close Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AlertCloseEvent(Alert target, Map<String, Object> detail) {
    super(target, detail);
  }
}
