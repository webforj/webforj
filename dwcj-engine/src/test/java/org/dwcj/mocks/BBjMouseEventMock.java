package org.dwcj.mocks;


import com.basis.bbj.proxies.event.BBjMouseEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;

/**
 * A Mock class for the BBjMouseEvent to test the corresponding eventsinks.
 */
public class BBjMouseEventMock implements BBjMouseEvent {

  @Override
  public BBjControl getControl() {
    return null;
  }

  @Override
  public String getEventName() {
    return "Mock Mouse Event";
  }

  @Override
  public byte[] getEventString() throws BBjException {
    return null;
  }

  @Override
  public int getClickCount() {
    return 1;
  }

  @Override
  public int getLegacyMouseButton() {
    return 1;
  }

  @Override
  public int getMouseButton() {
    return 2;
  }

  @Override
  public int getNativeMouseButton() {
    return 3;
  }

  @Override
  public int getNativeMouseButtons() {
    return 1;
  }

  @Override
  public BBjControl getOriginalControl() {
    return null;
  }

  @Override
  public int getScreenX() {
    return 10;
  }

  @Override
  public int getScreenY() {
    return 20;
  }

  @Override
  public int getX() {
    return 30;
  }

  @Override
  public int getY() {
    return 40;
  }

  @Override
  public boolean isAltDown() {
    return false;
  }

  @Override
  public boolean isCmdDown() {
    return false;
  }

  @Override
  public boolean isControlDown() {
    return false;
  }

  @Override
  public boolean isMetaDown() {
    return false;
  }

  @Override
  public boolean isShiftDown() {
    return true;
  }

}
