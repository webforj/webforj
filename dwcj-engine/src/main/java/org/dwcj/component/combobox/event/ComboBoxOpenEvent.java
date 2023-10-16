package org.dwcj.component.combobox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.combobox.ComboBox;

public class ComboBoxOpenEvent implements ControlEvent {

  private final ComboBox control;

  public ComboBoxOpenEvent(ComboBox cTextComboBox) {
    this.control = cTextComboBox;
  }

  @Override
  public ComboBox getControl() {
    return control;
  }

}
