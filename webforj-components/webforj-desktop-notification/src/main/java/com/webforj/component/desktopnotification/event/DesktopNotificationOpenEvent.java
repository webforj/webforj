package com.webforj.component.desktopnotification.event;

import com.webforj.component.desktopnotification.DesktopNotification;

/**
 * The event which is triggered when the notification is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class DesktopNotificationOpenEvent extends DesktopNotificationEvent {

  /**
   * Constructs a new DesktopNotificationOpenEvent.
   *
   * @param source the component which triggered the event.
   */
  public DesktopNotificationOpenEvent(DesktopNotification source) {
    super(source);
  }
}
