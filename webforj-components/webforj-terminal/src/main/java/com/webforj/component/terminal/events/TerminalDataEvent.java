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
 * Fired when the terminal data event fires. In a typical setup, this should be passed on to the
 * backing pty.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@EventName(value = "dwc-data")
@EventOptions(filter = "event.target.isSameNode(component)",
    data = {@EventData(key = "value", exp = "btoa(event.detail)")})
public final class TerminalDataEvent extends ComponentEvent<Terminal> {

  /**
   * Creates a new TerminalDateEvent.
   *
   * @param component the component
   * @param detail the detail of the event
   */
  public TerminalDataEvent(Terminal component, Map<String, Object> detail) {
    super(component, detail);
  }

  /**
   * Returns the data that was sent.
   *
   * @return the data that was sent
   */
  public String getValue() {
    String value = (String) this.getEventMap().get("value");
    return new String(Base64.getDecoder().decode(value), StandardCharsets.UTF_8);
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

