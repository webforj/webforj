package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import java.util.ArrayList;
import java.util.List;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;

/**
 * The class to implement the RadioGroup in order to group radioButtons.
 */
public class RadioGroup extends AbstractDwcComponent {

  private List<RadioButton> radioButtonList = new ArrayList<>();

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      ctrl = (BBjControl) w.addRadioGroup();
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Constructor taking as many RadioButtons as possible in RadioGroup.
   */
  public RadioGroup addRadioGroup(RadioButton... buttons) {
    radioButtonList.addAll(List.of(buttons));
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
  public RadioButton getSelected() {
    for (RadioButton radioButton : radioButtonList) {
      if (Boolean.TRUE.equals(radioButton.isChecked())) {
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
  public RadioGroup remove(RadioButton button) {
    radioButtonList.removeIf(radioButton -> radioButton == button);
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
