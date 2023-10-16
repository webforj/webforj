package org.dwcj.component.choicebox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public final class ChoiceBoxChangeEvent implements ControlEvent {

  private final ChoiceBox control;

  public ChoiceBoxChangeEvent(ChoiceBox comboBox) {
    this.control = comboBox;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
