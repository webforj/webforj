package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import java.util.ArrayList;
import java.util.List;

import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;

/**
 * The class to implement the RadioGroup in order to group radioButtons.
 */
public class RadioGroup extends AbstractDwcComponent {
  private List<RadioButton> radioButtonList = new ArrayList<>();
  private BBjRadioGroup radioGroup;

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      ctrl = (BBjControl) w.addRadioGroup();
      radioGroup = (BBjRadioGroup) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Constructor taking as many RadioButtons as possible in RadioGroup.
   */
  public RadioGroup addRadioGroup(RadioButton... buttons) throws BBjException {
    if (this.ctrl != null) {
      radioButtonList.addAll(List.of(buttons));
      for (RadioButton radioButton : radioButtonList) {
        radioGroup.add(radioButton.bbjRadioButton);
      }
    }
    return this;
  }

  /**
   * Empty constructor.
   */
  public RadioGroup addRadioGroup() {
    return this;
  }

  /**
   * Get the selected RadioButton from RadioGroup.
   *
   * @return The selected RadioButton.
   */
    public RadioButton getSelected() throws BBjException {
      radioGroup.getSelected();
      for (RadioButton radioButton : radioButtonList) {
        if(radioButton.isChecked()) {
          return radioButton;
        }
      }
      return null;
    }


  /**
   * Remove the RadioButton given as an Argument.
   *
   * @return the component itself.
   */
  public RadioGroup remove(RadioButton button) throws BBjException {
    radioGroup.remove(button.bbjRadioButton);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();
  }

}
