package com.webforj.component.desktopnotification.event;

import com.webforj.component.desktopnotification.DesktopNotification;

/**
 * The event which is triggered when the notification is errored.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class DesktopNotificationErrorEvent extends DesktopNotificationEvent {

  /**
   * Constructs a new DesktopNotificationErrorEvent.
   *
   * @param source the component which triggered the event.
   */
  public DesktopNotificationErrorEvent(DesktopNotification source) {
    super(source);
  }
}
