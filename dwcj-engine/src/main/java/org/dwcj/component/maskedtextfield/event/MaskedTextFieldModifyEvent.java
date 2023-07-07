package org.dwcj.component.maskedtextfield.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.maskedtextfield.MaskedTextField;

public class MaskedTextFieldModifyEvent implements ComponentEvent {

  private final MaskedTextField control;

  public MaskedTextFieldModifyEvent(MaskedTextField stringEditBox) {
    this.control = stringEditBox;
  }

  @Override
  public MaskedTextField getControl() {
    return control;
  }

  public String toString() {
    return "Event: StringEditBox modified";
  }

}
