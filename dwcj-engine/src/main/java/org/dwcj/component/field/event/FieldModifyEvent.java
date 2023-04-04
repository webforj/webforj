package org.dwcj.component.field.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.field.TextBox;

public class FieldModifyEvent implements ComponentEvent {

  private final TextBox control;

  public FieldModifyEvent(TextBox tBox) {
    this.control = tBox;
  }

  @Override
  public TextBox getControl() {
    return control;
  }

  public String toString() {
    return "Event: TextBox modified";
  }

}
