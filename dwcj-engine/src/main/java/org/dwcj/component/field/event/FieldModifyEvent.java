package org.dwcj.component.field.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.field.Field;

public class FieldModifyEvent implements ComponentEvent {

  private final Field control;

  public FieldModifyEvent(Field tBox) {
    this.control = tBox;
  }

  @Override
  public Field getControl() {
    return control;
  }

  public String toString() {
    return "Event: TextBox modified";
  }

}
