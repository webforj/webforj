package org.dwcj.component.choicebox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public class ChoiceBoxOpenEvent implements ControlEvent {

  private final ChoiceBox control;

  public ChoiceBoxOpenEvent(ChoiceBox cComboBox) {
    this.control = cComboBox;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
