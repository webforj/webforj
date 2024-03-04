package com.webforj.addons.terminal.events;

import com.webforj.addons.terminal.Terminal;
import com.webforj.component.Component;
import com.webforj.component.ControlEvent;

import java.util.Map;

public final class TerminalKeyEvent implements ControlEvent, TerminalKey {

  private final Terminal ctrl;
  private final Map<String, String> eventMap;

  public TerminalKeyEvent(Terminal t, Map<String, String> getEventMap) {
    this.ctrl = t;
    this.eventMap = getEventMap;
  }

  @Override
  public Component getControl() {
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
