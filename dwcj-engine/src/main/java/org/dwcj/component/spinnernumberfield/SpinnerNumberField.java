package org.dwcj.component.spinnernumberfield;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.maskednumberfield.MaskedNumberField;
import org.dwcj.component.window.Window;
import org.dwcj.concern.legacy.LegacyHasEnable;
import org.dwcj.concern.legacy.LegacyHasMouseWheelCondition;
import org.dwcj.utilities.BBjFunctionalityHelper;

public final class SpinnerNumberField extends MaskedNumberField
    implements LegacyHasMouseWheelCondition {

  private BBjInputNSpinner numBoxS;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public SpinnerNumberField() {
    this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
  }

  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addInputNSpinner(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1, flags);
      numBoxS = (BBjInputNSpinner) this.numBox;
      this.numBox = (BBjInputNSpinner) this.control;
      super.onAttach();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }


  @Override
  public MouseWheelCondition getScrollWheelBehavior() {
    return this.mouseWheelCondition;
  }

  @Override
  public SpinnerNumberField setScrollWheelBehavior(MouseWheelCondition condition) {
    if (this.control != null) {
      try {
        numBoxS.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void onAttach() {
    super.onAttach();

    if (this.mouseWheelCondition != MouseWheelCondition.DEFAULT) {
      this.setScrollWheelBehavior(this.mouseWheelCondition);
    }

  }

}
