package org.dwcj.component.maskednumberfield.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.maskednumberfield.MaskedNumberField;

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
