package com.webforj.component.desktopnotification.event;

import com.webforj.component.desktopnotification.DesktopNotification;
import java.util.EventObject;

/**
 * The base class for all DesktopNotification events.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class DesktopNotificationEvent extends EventObject {

  /**
   * Constructs a new DesktopNotificationEvent.
   *
   * @param source the component which triggered the event.
   */
  public DesktopNotificationEvent(DesktopNotification source) {
    super(source);
  }

  /**
   * Gets the component which triggered the event.
   *
   * @return the component which triggered the event.
   */
  public DesktopNotification getComponent() {
    return getSource();
  }

  /**
   * Gets the source of the event.
   *
   * @return the source of the event.
   */
  @Override
  public DesktopNotification getSource() {
    return (DesktopNotification) super.getSource();
  }
}
