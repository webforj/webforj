package org.dwcj.component.combobox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.combobox.ComboBox;


public class ComboBoxEditModifyEvent implements ControlEvent {

  private final ComboBox control;

  public ComboBoxEditModifyEvent(ComboBox cTextComboBox) {
    this.control = cTextComboBox;
  }

  @Override
  public ComboBox getControl() {
    return control;
  }
}
