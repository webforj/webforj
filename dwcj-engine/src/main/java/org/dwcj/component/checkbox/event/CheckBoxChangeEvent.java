package org.dwcj.component.checkbox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.checkbox.CheckBox;

public final class CheckBoxChangeEvent implements ComponentEvent {

  private final CheckBox control;

  private boolean isChecked = false;

  public CheckBoxChangeEvent(CheckBox cCheckBox, boolean checked) {
    this.isChecked = checked;
    this.control = cCheckBox;
  }

  public boolean isChecked() {
    return isChecked;
  }

  @Override
  public CheckBox getControl() {
    return control;
  }
}
