package com.webforj.component.event.mocks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjInputT;
import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.startup.type.BBjException;

/**
 * A mock class for the BBjEditModifyEvent to test the corresponding event sinks.
 */
public class BBjEditModifyEventMock implements BBjEditModifyEvent {

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
  public BBjCEdit getCEdit() throws BBjException {
    return null;
  }

  @Override
  public BBjEditBox getEditBox() throws BBjException {
    return null;
  }

  @Override
  public BBjInputD getInputD() throws BBjException {
    return null;
  }

  @Override
  public BBjInputE getInputE() throws BBjException {
    return null;
  }

  @Override
  public BBjInputN getInputN() throws BBjException {
    return null;
  }

  @Override
  public BBjInputT getInputT() throws BBjException {
    return null;
  }

  @Override
  public BBjListEdit getListEdit() throws BBjException {
    return null;
  }

  @Override
  public String getText() {
    return "text";
  }
}
