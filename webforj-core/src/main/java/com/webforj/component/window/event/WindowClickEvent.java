package com.webforj.component.window.event;

import com.webforj.component.event.MouseEvent;
import com.webforj.component.window.Window;
import java.util.Map;

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
