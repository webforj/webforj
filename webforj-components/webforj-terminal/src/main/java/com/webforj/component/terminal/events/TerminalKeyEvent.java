package com.webforj.component.terminal.events;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.terminal.Terminal;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;

/**
 * Fired when a key is pressed.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@EventName(value = "dwc-key")
@EventOptions(filter = "event.target.isSameNode(component)",
    data = {@EventData(key = "code", exp = "event.detail.event.code"),
        @EventData(key = "key", exp = "event.detail.event.key"),
        @EventData(key = "value", exp = "btoa(event.detail.value)"),
        @EventData(key = "altKey", exp = "event.detail.event.altKey"),
        @EventData(key = "controlKey", exp = "event.detail.event.ctrlKey"),
        @EventData(key = "shiftKey", exp = "event.detail.event.shiftKey")})
public final class TerminalKeyEvent extends ComponentEvent<Terminal> {

  /**
   * Creates a new TerminalKeyEvent.
   *
   * @param component the component
   * @param detail the detail of the event
   */
  public TerminalKeyEvent(Terminal component, Map<String, Object> detail) {
    super(component, detail);
  }

  /**
   * Returns the code for the key that was pressed as defined in the browser.
   *
   * @return the code for the key that was pressed
   */
  public String getCode() {
    return (String) this.getEventMap().get("code");
  }

  /**
   * Returns the key that was pressed.
   *
   * @return the key that was pressed
   */
  public String getKey() {
    return (String) this.getEventMap().get("key");
  }

  /**
   * Returns the value of the key that was pressed.
   *
   * @return the value of the key that was pressed
   */
  public String getValue() {
    String value = (String) this.getEventMap().get("value");
    return new String(Base64.getDecoder().decode(value), StandardCharsets.UTF_8);
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public boolean isAltKey() {
    return (boolean) this.getEventMap().get("altKey");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public boolean isControlKey() {
    return (boolean) this.getEventMap().get("controlKey");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public boolean isShiftKey() {
    return (boolean) this.getEventMap().get("shiftKey");
  }

  /**
   * Returns the terminal component.
   *
   * @return the terminal component
   */
  @Override
  public Terminal getComponent() {
    return (Terminal) super.getComponent();
  }
}
