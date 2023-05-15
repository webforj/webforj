package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.List;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;


/**
 * The class to implement the RadioGroup in order to group radioButtons.
 */
public class RadioGroup extends AbstractDwcComponent {
  private final List<RadioButton> radioButtonList = new ArrayList<>();
  private BBjRadioGroup bbjRadioGroup;
  private AbstractWindow pi;

  @Override
  protected void create(AbstractWindow p) {
    try {
      pi = p;
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(pi);
      bbjRadioGroup = w.addRadioGroup();
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Empty constructor.
   */
  public RadioGroup add() {
    return this;
  }

  /**
   * Constructor taking as many RadioButtons as possible in RadioGroup.
   */
  public RadioGroup add(RadioButton... buttons) throws BBjException {
    radioButtonList.addAll(List.of(buttons));

    if (this.bbjRadioGroup != null) {
      for (RadioButton radioButton : radioButtonList) {
        if (Boolean.FALSE.equals(radioButton.getCaughtUp())) {
          pi.add(radioButton);
        }
        try {
          BBjControl chosenRadioButton = ComponentAccessor.getDefault().getBBjControl(radioButton);
          bbjRadioGroup.add((BBjRadioButton) chosenRadioButton);
        } catch (IllegalAccessException e) {
          Environment.logError(e);
        }
      }
    }
    return this;
  }

  /**
   * Remove the RadioButton given as an Argument.
   *
   * @return the component itself.
   */
  public RadioGroup remove(RadioButton radioButton) throws BBjException {
    if (this.bbjRadioGroup != null) {
      try {
        BBjControl chosenRadioButton = ComponentAccessor.getDefault().getBBjControl(radioButton);
        bbjRadioGroup.remove((BBjRadioButton) chosenRadioButton);
      } catch (IllegalAccessException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  /**
   * Get the selected RadioButton from RadioGroup.
   *
   * @return The selected RadioButton.
   */
  public RadioButton getSelected() {
    for (RadioButton radioButton : radioButtonList) {
      try {
        BBjControl chosenRadioButton = ComponentAccessor.getDefault().getBBjControl(radioButton);
        String bbjRadioButtonId = String.valueOf(bbjRadioGroup.getSelected().getID());
        if (String.valueOf(chosenRadioButton.getID()).equals(bbjRadioButtonId)) {
          return radioButton;
        }
      } catch (BBjException | IllegalAccessException e) {
        Environment.logError(e);
      }
    }
    return null;
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
