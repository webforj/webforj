package com.webforj.component.maskeddatefield.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.maskeddatefield.MaskedDateField;

public final class MaskedDateFieldModifyEvent implements ControlEvent {
  private final MaskedDateField control;

  public MaskedDateFieldModifyEvent(MaskedDateField cDateEditBox) {
    this.control = cDateEditBox;
  }

  @Override
  public MaskedDateField getControl() {
    return control;
  }

  public String toString() {
    return "Event: DateEditBox modified";
  }
}
