package org.dwcj.component.window.event;

import java.util.Map;
import org.dwcj.component.event.MouseEvent;
import org.dwcj.component.window.Window;

/**
 * An event that is fired when the window is clicked.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class WindowClickEvent extends MouseEvent {

  public WindowClickEvent(Window component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Returns the number of clicks associated with this event.
   *
   * @return The number of clicks associated with this event.
   */
  public int getClickCount() {
    return (int) this.getEventMap().get("clickCount");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Window getComponent() {
    return (Window) super.getComponent();
  }
}
