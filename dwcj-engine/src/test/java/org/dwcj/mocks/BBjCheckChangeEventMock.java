package org.dwcj.mocks;

import com.basis.bbj.proxies.event.BBjCheckChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;

/**
 * A mock for the bbj change event.
 */
public class BBjCheckChangeEventMock implements BBjCheckChangeEvent {

  @Override
  public com.basis.bbj.proxies.sysgui.BBjRadioButton getRadioButton()
      throws com.basis.startup.type.BBjException {
    return null;
  }

  @Override
  public com.basis.bbj.proxies.sysgui.BBjCheckBox getCheckBox()
      throws com.basis.startup.type.BBjException {
    return null;
  }

  @Override
  public boolean isChecked() {
    return true;
  }

  @Override
  public boolean isIndeterminate() {
    return false;
  }

  @Override
  public BBjControl getControl() {
    return null;
  }

  @Override
  public byte[] getEventString() {
    return null;
  }

  @Override
  public String getEventName() {
    return "test";
  }
}
