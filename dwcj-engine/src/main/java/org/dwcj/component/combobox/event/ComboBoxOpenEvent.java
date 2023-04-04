package org.dwcj.component.combobox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.ComboBox;

public class ComboBoxOpenEvent implements ComponentEvent {

  private final ComboBox control;

  public ComboBoxOpenEvent(ComboBox cTextComboBox) {
    this.control = cTextComboBox;
  }

  @Override
  public ComboBox getControl() {
    return control;
  }

}
