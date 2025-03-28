package com.webforj.component.desktopnotification.event;

import com.webforj.component.desktopnotification.DesktopNotification;

/**
 * The event which is triggered when the notification is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class DesktopNotificationCloseEvent extends DesktopNotificationEvent {

  /**
   * Constructs a new DesktopNotificationCloseEvent.
   *
   * @param source the component which triggered the event.
   */
  public DesktopNotificationCloseEvent(DesktopNotification source) {
    super(source);
  }
}
