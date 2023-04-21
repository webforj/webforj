package org.dwcj.mocks;

import com.basis.bbj.proxies.event.BBjEditKeypressEvent;
import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;

/** A mock class for the BBjEditKeypressEvent to test corresponding event sink. */
public class BBjKeypressEventMock implements BBjEditKeypressEvent {

  @Override
  public BBjControl getControl() {
    return null;
  }

  @Override
  public String getEventName() {
    return null;
  }

  @Override
  public byte[] getEventString() throws BBjException {
    return null;
  }

  @Override
  public boolean getCapsLock() {
    return false;
  }

  @Override
  public int getKeyCode() {
    return 15;
  }

  @Override
  public int getKeyLocation() {
    return 0;
  }


  @Override
  public boolean getNumLock() {
    return false;
  }


  @Override
  public boolean isAltDown() {
    return true;
  }

  @Override
  public boolean isCmdDown() {
    return true;
  }

  @Override
  public boolean isControlDown() {
    return true;
  }

  @Override
  public boolean isShiftDown() {
    return true;
  }

  @Override
  public int getKeyCodeWithFlags() {
    return 20;
  }

  @Override
  public int getModifiersEx() {
    return 21;
  }

  @Override
  public BBjCEdit getCEdit() throws BBjException {
    return null;
  }

  @Override
  public BBjEditBox getEditBox() throws BBjException {
    return null;
  }
}
