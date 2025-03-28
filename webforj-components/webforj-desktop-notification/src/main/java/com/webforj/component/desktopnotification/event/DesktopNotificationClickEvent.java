package com.webforj.component.desktopnotification.event;

import com.webforj.component.desktopnotification.DesktopNotification;

/**
 * The event which is triggered when the notification is clicked.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class DesktopNotificationClickEvent extends DesktopNotificationEvent {

  /**
   * Constructs a new DesktopNotificationClickEvent.
   *
   * @param source the component which triggered the event.
   */
  public DesktopNotificationClickEvent(DesktopNotification source) {
    super(source);
  }
}
