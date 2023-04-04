package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public class ChoiceBoxOpenEvent implements ComponentEvent {

  private final ChoiceBox control;

  public ChoiceBoxOpenEvent(ChoiceBox cComboBox) {
    this.control = cComboBox;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
