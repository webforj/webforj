package org.dwcj.component.event;

import com.basis.bbj.proxies.sysgui.BBjControl;
import java.util.Map;
import org.dwcj.component.AbstractComponent;


/**
 * An event that is fired when the mouse enters a component.
 */
public class MouseEnterEvent extends Event<AbstractComponent> {

  /**
   * Creates a new event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public MouseEnterEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  public int getLegacyMouseButton() {
    return (int) this.getEventMap().get("legacyMouseButton");
  }

  public int getMouseButton() {
    return (int) this.getEventMap().get("mouseButton");
  }

  public int getNativeMouseButton() {
    return (int) this.getEventMap().get("nativeMouseButton");
  }

  public BBjControl getOriginalControl() {
    return (BBjControl) this.getEventMap().get("originalControl");
  }

  public int getScreenX() {
    return (int) this.getEventMap().get("screenX");
  }

  public int getScreenY() {
    return (int) this.getEventMap().get("screenY");
  }

  public int getX() {
    return (int) this.getEventMap().get("x");
  }

  public int getY() {
    return (int) this.getEventMap().get("y");
  }

  public boolean isAltDown() {
    return (boolean) this.getEventMap().get("altDown");
  }

  public boolean isCmdDown() {
    return (boolean) this.getEventMap().get("cmdDown");
  }

  public boolean isControlDown() {
    return (boolean) this.getEventMap().get("controlDown");
  }

  public boolean isShiftDown() {
    return (boolean) this.getEventMap().get("shiftDown");
  }
}
