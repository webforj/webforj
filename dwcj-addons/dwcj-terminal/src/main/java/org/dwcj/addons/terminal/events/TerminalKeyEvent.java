package org.dwcj.addons.terminal.events;

import org.dwcj.addons.terminal.Terminal;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.ComponentEvent;

import java.util.Map;

public final class TerminalKeyEvent implements ComponentEvent, TerminalKey {

  private final Terminal ctrl;
  private final Map<String, String> eventMap;

  public TerminalKeyEvent(Terminal t, Map<String, String> getEventMap) {
    this.ctrl = t;
    this.eventMap = getEventMap;
  }

  @Override
  public AbstractComponent getControl() {
    return ctrl;
  }

  @Override
  public boolean getAltDown() {
    return false;
  }

  @Override
  public boolean getCtrlDown() {
    return false;
  }

  @Override
  public boolean getShiftDown() {
    return false;
  }

  @Override
  public boolean getMetaDown() {
    return false;
  }

  @Override
  public String getKey() {
    return (String) eventMap.get("key");
  }

  @Override
  public String getCode() {
    return (String) eventMap.get("keyCode");
  }

  @Override
  public int getKeyCode() {
    return Integer.parseInt(eventMap.get("keyCode"));
  }
}
