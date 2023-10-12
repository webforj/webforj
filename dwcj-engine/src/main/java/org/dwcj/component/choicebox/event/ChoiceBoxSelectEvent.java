package org.dwcj.component.choicebox.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public final class ChoiceBoxSelectEvent implements ControlEvent {

  private final ChoiceBox control;

  private Object key;

  public ChoiceBoxSelectEvent(ChoiceBox cComboBox) {
    this.control = cComboBox;
    this.key = control.getSelectedItem().getKey();
  }

  public void setKey(Object key) {
    this.key = key;
  }

  public Object getKey() {
    return key;
  }

  @Override
  public ChoiceBox getControl() {
    return control;
  }
}
