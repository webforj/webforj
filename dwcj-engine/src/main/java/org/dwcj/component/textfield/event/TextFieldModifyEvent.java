package org.dwcj.component.textfield.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.textfield.TextField;

public class TextFieldModifyEvent implements ComponentEvent {

  private final TextField control;

  public TextFieldModifyEvent(TextField stringEditBox) {
    this.control = stringEditBox;
  }

  @Override
  public TextField getControl() {
    return control;
  }

  public String toString() {
    return "Event: StringEditBox modified";
  }

}
