package org.dwcj.component.event.mocks;

import java.util.HashMap;
import java.util.Map;
import com.basis.bbj.proxies.event.BBjCheckChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;

/**
 * A mock for the bbj change event.
 */
public class BBjCheckChangeEventMock implements BBjCheckChangeEvent {

  @Override
  public BBjRadioButton getRadioButton() throws com.basis.startup.type.BBjException {
    return null;
  }

  @Override
  public BBjCheckBox getCheckBox() throws com.basis.startup.type.BBjException {
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

  @Override
  public Map<String, Object> getClientMap() {
    return new HashMap<String, Object>();
  }
}
