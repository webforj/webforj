package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
      if (radioButton.isChecked()) {
        return radioButton;
      }
    }
    return null;
  }

}
