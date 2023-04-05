package org.dwcj.component.radiobutton.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.radiobutton.RadioButton;

public final class RadioButtonCheckEvent implements ComponentEvent {

  private final RadioButton control;

  private boolean isChecked = false;

  public RadioButtonCheckEvent(RadioButton rButton, boolean checked) {
    this.isChecked = checked;
    this.control = rButton;
  }

  public boolean isChecked() {
    return isChecked;
  }

  @Override
  public RadioButton getControl() {
    return control;
  }

}
