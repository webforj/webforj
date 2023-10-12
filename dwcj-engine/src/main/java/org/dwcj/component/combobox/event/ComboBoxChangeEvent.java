package org.dwcj.component.combobox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.combobox.ComboBox;


public class ComboBoxChangeEvent implements ControlEvent {

  private final ComboBox control;

  public ComboBoxChangeEvent(ComboBox tComboBox) {
    this.control = tComboBox;
  }

  @Override
  public ComboBox getControl() {
    return control;
  }
}
