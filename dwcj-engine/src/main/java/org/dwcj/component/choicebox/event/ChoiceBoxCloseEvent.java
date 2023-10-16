package org.dwcj.component.choicebox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public class ChoiceBoxCloseEvent implements ControlEvent {

  private final ChoiceBox control;

  public ChoiceBoxCloseEvent(ChoiceBox cComboBox) {
    this.control = cComboBox;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
