package com.webforj.component.maskednumberfield.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.maskednumberfield.MaskedNumberField;

public class MaskedNumberFieldModifyEvent implements ControlEvent {

  private final MaskedNumberField control;

  public MaskedNumberFieldModifyEvent(MaskedNumberField nBox) {
    this.control = nBox;
  }

  @Override
  public MaskedNumberField getControl() {
    return control;
  }

  public String toString() {
    return "Event: DateEditBox modified";
  }

}
