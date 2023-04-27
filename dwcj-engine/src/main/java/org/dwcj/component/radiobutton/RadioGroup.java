package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import java.util.ArrayList;
import java.util.List;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;

public class RadioGroup extends AbstractDwcComponent{

  private BBjRadioGroup bbjRadioGroup;
  private List<RadioButton> radioButtonList = new ArrayList<>();

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      ctrl = (BBjControl) w.addRadioGroup();
      this.bbjRadioGroup = (BBjRadioGroup) ctrl;
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public RadioGroup addRadioGroup(RadioButton... buttons){
    radioButtonList.addAll(List.of(buttons));
    return this;
  }

  public RadioGroup addRadioGroup() {
    return this;
  }

  public RadioButton getSelected() {
    for (RadioButton radioButton : radioButtonList) {
      if (Boolean.TRUE.equals(radioButton.isChecked())) {
        return radioButton;
      }
    }
    return null;
  }

  public RadioGroup remove(RadioButton button) {
    radioButtonList.removeIf(radioButton -> radioButton == button);
    return this;
  }

}
