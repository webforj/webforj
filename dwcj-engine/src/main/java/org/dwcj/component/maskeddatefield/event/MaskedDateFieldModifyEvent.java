package org.dwcj.component.maskeddatefield.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.maskeddatefield.MaskedDateField;

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
