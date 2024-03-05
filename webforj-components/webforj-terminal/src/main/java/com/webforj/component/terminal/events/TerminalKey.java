package com.webforj.component.terminal.events;

public interface TerminalKey {
  boolean getAltDown();

  boolean getCtrlDown();

  boolean getShiftDown();

  boolean getMetaDown();

  String getKey();

  String getCode();

  int getKeyCode();

}
