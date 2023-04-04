package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public class ChoiceBoxCloseEvent implements ComponentEvent {

  private final ChoiceBox control;

  public ChoiceBoxCloseEvent(ChoiceBox cComboBox) {
    this.control = cComboBox;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
