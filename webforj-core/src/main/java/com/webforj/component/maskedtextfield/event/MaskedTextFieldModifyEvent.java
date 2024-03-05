package com.webforj.component.maskedtextfield.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.maskedtextfield.MaskedTextField;

public class MaskedTextFieldModifyEvent implements ControlEvent {

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
