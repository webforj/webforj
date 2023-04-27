package org.dwcj.component.progressbar;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.window.AbstractWindow;

public final class ProgressBar extends AbstractDwcComponent implements HasEnable{

  private BBjProgressBar bbjProgressBar;

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }

  private Boolean indeterminate = false;
  private Integer maximum = 100;
  private Integer minimum = 0;
  /* 0 for horizontal, 1 for vertical */
  private Integer orientation = 0;
  private Boolean stringPainted = true;
  private String text = "0%";
  private Integer value = 0;


  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      ctrl = w.addProgressBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1);
      bbjProgressBar = (BBjProgressBar) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * This method returns the maximum range of theProgressBar control.
   *
   * @return Returns the maximum range of the progress bar.
   */
  public Integer getMaximum() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.getMaximum();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.maximum;
  }

  /**
   * This method returns the minimum range of theProgressBar control.
   *
   * @return Returns the minimum range of the progress bar.
   */
  public Integer getMinimum() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.getMinimum();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.minimum;
  }


  /**
   * This method returns the orientation of the ProgressBar control.
   *
   * @return Returns 0 if horizontal, 1 if vertical.
   */
  public Integer getOrientation() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.getOrientation();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.orientation;
  }

  /**
   * This method returns the text of aProgressBar control.
   *
   * @return Returns the text (label) of the progress bar control.
   */
  @Override
  public String getText() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.getText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.text;
  }

  /**
   * This method returns the current value of a ProgressBar control.
   *
   * @return Returns the current value of the progress bar control.
   */
  public Integer getValue() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.getValue();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.value;
  }

  /**
   * This method returns whether the ProgressBar control.
   *
   * @return Returns whether the progress bar is indeterminate (false = specific range, true =
   *         indeterminate).
   */
  public Boolean isIndeterminate() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.isIndeterminate();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.indeterminate;
  }

  /**
   * This method returns whether the ProgressBar control will display a label (defaults to %
   * complete).
   *
   * @return Returns whether the progress bar will show a label (false = no label, true = label will
   *         be displayed).
   */
  public Boolean isStringPainted() {
    if (this.ctrl != null) {
      try {
        return bbjProgressBar.isStringPainted();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.stringPainted;
  }

  /**
   * This method sets whether the ProgressBar control is indeterminate. This option is not available
   * on all platforms.
   *
   * @param indeterminate - Sets whether the progress bar is indeterminate (false = Progress bar has
   *        a fixed range, which can be retrieved with getMinimum() and getMaximum(), true =
   *        Progress bar is indeterminate, indicating that the duration of the task is not yet
   *        known.)
   * @return Returns this
   */
  public ProgressBar setIndeterminate(Boolean indeterminate) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setIndeterminate(indeterminate);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  /**
   * This method sets the maximum range for the ProgressBar control.
   *
   * @param maximum - Specifies the maximum range of the BBjProgressBar control.
   * @return Returns this
   */
  public ProgressBar setMaximum(Integer maximum) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setMaximum(maximum);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.maximum = maximum;
    return this;
  }

  /**
   * This method sets the minimum range for the ProgressBar control.
   *
   * @param minimum - Specifies the minimum range of the BBjProgressBar control.
   * @return Returns this
   */
  public ProgressBar setMinimum(Integer minimum) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setMinimum(minimum);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.minimum = minimum;
    return this;
  }

  /**
   * This method sets the orientation of the ProgressBar control to HORIZONTAL or VERTICAL.
   *
   * @param orientation - Specifies the orientation as HORIZONTAL or VERTICAL.
   * @return Returns this
   */
  public ProgressBar setOrientation(Integer orientation) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setOrientation(orientation);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.orientation = orientation;
    return this;
  }

  /**
   * This method determines whether the ProgressBar control will show a label.
   *
   * @param stringPainted - Specifies whether the progress bar should display a label (false = Not
   *        painted, 1 = Painted)
   * @return Returns this
   */
  public ProgressBar setStringPainted(Boolean stringPainted) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setStringPainted(stringPainted);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.stringPainted = stringPainted;
    return this;
  }

  /**
   * This method sets the text (label) of a ProgressBar control.
   *
   * @param text - Specifies the text to be displayed on the BBjProgressBar. If text is set to "",
   *        the progress bar will display percentage complete in the format "XX%".
   * @return Returns this
   */
  public ProgressBar setProgressBarText(String text) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setText(text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.text = text;
    return this;
  }

  /**
   * This method sets the value of a ProgressBar control.
   *
   * @param value - Specifies the value of the control.
   * @return Returns this
   */
  public ProgressBar setValue(Integer value) {
    if (this.ctrl != null) {
      try {
        bbjProgressBar.setValue(value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.value = value;
    return this;
  }



  @Override
  public ProgressBar setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public ProgressBar setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public ProgressBar setEnabled(Boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override 
  public Boolean isEnabled(){
    return super.isComponentEnabled();
  }

  @Override
  public ProgressBar setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public ProgressBar setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public ProgressBar setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public ProgressBar addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public ProgressBar removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }


  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (Boolean.TRUE.equals(this.indeterminate)) {
      this.setIndeterminate(this.indeterminate);
    }

    if (this.maximum != 100) {
      this.setMaximum(this.maximum);
    }

    if (this.minimum != 0) {
      this.setMinimum(this.minimum);
    }

    if (this.orientation != 0) {
      this.setOrientation(this.orientation);
    }

    if (Boolean.FALSE.equals(this.stringPainted)) {
      this.setStringPainted(this.stringPainted);
    }

    if (!"0%".equals(this.text)) {
      this.setText(this.text);
    }

    if (this.value != 0) {
      this.setValue(this.value);
    }

  }


}
