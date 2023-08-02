package org.dwcj.component.slider;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.slider.event.SliderScrollEvent;
import org.dwcj.component.slider.sink.SliderScrollEventSink;
import org.dwcj.component.textarea.TextArea;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.concern.HasEnable;
import org.dwcj.concern.HasFocus;
import org.dwcj.concern.HasMouseWheelCondition;
import org.dwcj.concern.HasTabTraversal;
import org.dwcj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public final class Slider extends AbstractDwcComponent
    implements HasFocus, HasMouseWheelCondition, HasTabTraversal, HasEnable {

  private BBjSlider bbjSlider;


  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
  }

  public enum Orientation {
    HORIZONTAL("horizontal"), VERTICAL("vertical");

    public final String value;

    private Orientation(String value) {
      this.value = value;
    }
  }

  private ArrayList<Consumer<SliderScrollEvent>> callbacks = new ArrayList<>();
  private SliderScrollEventSink scrollEventSink;

  private Orientation orientation = Orientation.HORIZONTAL;
  private Boolean inverted = false;
  private Integer majorTickSpacing = 1;
  private Integer minorTickSpacing = 1;
  private Integer maximum = 100;
  private Integer minimum = 0;
  private Boolean paintLabels = false;
  private Boolean paintTicks = false;
  private Boolean snapToTicks = false;
  private Integer value = 0;


  public Slider() {
    this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
    this.tabTraversable = true;
  }

  @Override
  protected void create(AbstractWindow p) {

    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addHorizontalSlider(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_250, BASISNUMBER_250, flags);
      bbjSlider = (BBjSlider) control;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }

  }

  /**
   * Sets a callback to fire when the slider control is scrolled.
   *
   * @param callback Function written with behavior to be executed when the event is fired
   * @return The object itself
   */
  public Slider onScroll(Consumer<SliderScrollEvent> callback) {
    if (this.control != null) {
      if (this.scrollEventSink == null) {
        this.scrollEventSink = new SliderScrollEventSink(this);
      }
      this.scrollEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  /**
   * This method gets the orientation of the ProgressBar control. By default, the minimum value of a
   * vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider,
   * the minimum value is to the left and the maximum value is to the right. The orientation
   * reverses for inverted sliders.
   *
   * @return Returns whether the control orientation is inverted.
   */
  public Boolean isInverted() {
    if (this.control != null) {
      try {
        return bbjSlider.getInverted();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.inverted;
  }

  /**
   * This method returns the labels from a ProgressBar control.
   *
   * @return Returns a Java Map<Integer,String> structure, where each Integer key is the slider
   *         position of the corresponding String label.
   */
  public Map<Integer, String> getLabels() {
    if (this.control != null) {
      try {
        return bbjSlider.getLabels();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return new HashMap<>();
  }

  /**
   * This method queries the slider's major tick spacing.
   *
   * @return Returns the slider's major tick spacing.
   */
  public Integer getMajorTickSpacing() {
    if (this.control != null) {
      try {
        return bbjSlider.getMajorTickSpacing();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.majorTickSpacing;
  }

  /**
   * This method returns the maximum value of the ProgressBar control.
   *
   * @return Returns the maximum value of the control.
   */
  public Integer getMaximum() {
    if (this.control != null) {
      try {
        return bbjSlider.getMaximum();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.maximum;
  }

  /**
   * This method returns the minimum value of the ProgressBar control.
   *
   * @return Returns the minimum value of the control.
   */
  public Integer getMinimum() {
    if (this.control != null) {
      try {
        return bbjSlider.getMinimum();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.minimum;
  }

  /**
   * This method queries the minor tick spacing of the ProgressBar control.
   *
   * @return Returns the slider's minor tick spacing.
   */
  public Integer getMinorTickSpacing() {
    if (this.control != null) {
      try {
        return bbjSlider.getMinorTickSpacing();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.minorTickSpacing;
  }

  /**
   * This method returns the orientation of the ProgressBar control.
   *
   * @return Returns the orientation of the control (0 = HORIZONTAL, 1 = VERTICAL).
   */
  public Integer getOrientation() {
    if (this.control != null) {
      return bbjSlider.getOrientation();
    }
    if (this.orientation == Orientation.HORIZONTAL) {
      return 0;
    }
    return 1;
  }

  /**
   * This method queries whether to paint labels on the ProgressBar control.
   *
   * @return Returns whether labels are painted on this slider.
   */
  public Boolean isPaintLabels() {
    if (this.control != null) {
      try {
        return bbjSlider.getPaintLabels();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.paintLabels;
  }

  /**
   * This method queries whether to paint ticks on the ProgressBar control.
   *
   * @return Returns whether ticks are painted on this slider.
   */
  public Boolean isPaintTicks() {
    if (this.control != null) {
      try {
        return bbjSlider.getPaintTicks();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.paintTicks;
  }

  /**
   * This method queries whether a ProgressBar control should snap to the nearest tick when the user
   * drags the thumb.
   *
   * @return Returns whether the BBjSlider should snap to the nearest tick when the user drags the
   *         thumb.
   */
  public Boolean isSnapToTicks() {
    if (this.control != null) {
      try {
        return bbjSlider.getSnapToTicks();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.snapToTicks;
  }

  /**
   * This method returns the current value of the ProgressBar control.
   *
   * @return Returns the current value of the control.
   */
  public Integer getValue() {
    if (this.control != null) {
      try {
        return bbjSlider.getValue();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.value;
  }

  /**
   * This method sets the orientation of the ProgressBar control. By default, the minimum value of a
   * vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider,
   * the minimum value is to the left and the maximum value is to the right. The orientation
   * reverses for inverted sliders.
   *
   * @param inverted - Specifies whether the slider orientation is inverted.
   * @return Returns this
   */
  public Slider setInverted(Boolean inverted) {
    if (this.control != null) {
      try {
        bbjSlider.setInverted(inverted);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.inverted = inverted;
    return this;
  }

  /**
   * This method sets the custom labels for a ProgressBar control.
   *
   * @param labels - A Java Map<Integer,String> structure, where the Integer key is the slider
   *        position of the corresponding String label.
   * @return Returns this
   */
  public Slider setLabels(Map<Integer, String> labels) {
    if (this.control != null) {
      try {
        bbjSlider.setLabels(labels);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }

  /**
   * This method sets the major tick spacing for a ProgressBar control.
   *
   * @param prop - Specifies the major tick spacing.
   * @return Returns this
   */
  public Slider setMajorTickSpacing(Integer tick) {
    if (this.control != null) {
      try {
        bbjSlider.setMajorTickSpacing(tick);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.majorTickSpacing = tick;
    return this;
  }

  /**
   * This method sets the maximum value of the ProgressBar control.
   *
   * @param value - Specifies the maximum value.
   * @return Returns this
   */
  public Slider setMaximum(Integer maximum) {
    if (this.control != null) {
      try {
        bbjSlider.setMaximum(maximum);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.maximum = maximum;
    return this;
  }

  /**
   * This method sets the minimum value of the ProgressBar control.
   *
   * @param value - Specifies the minimum value.
   * @return Returns this
   */
  public Slider setMinimum(Integer minimum) {
    if (this.control != null) {
      try {
        bbjSlider.setMinimum(minimum);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.minimum = minimum;
    return this;
  }

  /**
   * This method sets the minor tick spacing of a ProgressBar control.
   *
   * @param tick - Specifies the minor tick spacing.
   * @return Returns this
   */
  public Slider setMinorTickSpacing(Integer tick) {
    if (this.control != null) {
      try {
        bbjSlider.setMinorTickSpacing(tick);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.minorTickSpacing = tick;
    return this;
  }

  /**
   * Will set the slider to vertical if the string "vertical" is passed. Defaults to horizontal for
   * any other string.
   *
   * @param orientation String "vertical" to set the slider's orientation to vertical, "horizontal"
   *        for horizontal(default)
   * @return The object itself
   */
  public Slider setOrientation(Orientation orientation) {
    if (this.control != null) {
      this.setAttribute("orientation", orientation.value);
    }
    this.orientation = orientation;
    return this;
  }

  /**
   * This method sets whether labels are painted on a ProgressBar control.
   *
   * @param paint - Specifies whether labels are painted on the
   * @return Returns this
   */
  public Slider setPaintLabels(Boolean paint) {
    if (this.control != null) {
      try {
        bbjSlider.setPaintLabels(paint);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.paintLabels = paint;
    return this;
  }

  /**
   * This method sets whether ticks are painted on a ProgressBar control.
   *
   * @param paint - Specifies whether ticks are painted on the control.
   * @return Returns this
   */
  public Slider setPaintTicks(Boolean paint) {
    if (this.control != null) {
      try {
        bbjSlider.setPaintTicks(paint);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.paintTicks = paint;
    return this;
  }

  /**
   * This method sets whether a ProgressBar control should snap to the nearest tick when the user
   * drags the thumb.
   *
   * @param snap - Specifies whether the control should snap to the nearest tick when the user drags
   *        the thumb.
   * @return Returns this
   */
  public Slider setSnapToTicks(Boolean snap) {
    if (this.control != null) {
      try {
        bbjSlider.setSnapToTicks(snap);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.snapToTicks = snap;
    return this;
  }

  /**
   * This method sets the value of the ProgressBar control.
   *
   * @param value - Specifies the slider value.
   * @return Returns this
   */
  public Slider setValue(Integer value) {
    if (this.control != null) {
      try {
        bbjSlider.setValue(value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.value = value;
    return this;
  }



  @Override
  public Slider focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        bbjSlider.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public Slider setTabTraversable(Boolean traverse) {
    if (this.control != null) {
      try {
        bbjSlider.setTabTraversable(traverse);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traverse;
    return this;
  }


  @Override
  public MouseWheelCondition getScrollWheelBehavior() {
    return this.mouseWheelCondition;
  }

  @Override
  public Slider setScrollWheelBehavior(MouseWheelCondition condition) {
    if (this.control != null) {
      try {
        bbjSlider.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this;
  }


  @Override
  public Slider setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Slider setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Slider setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public Slider setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Slider setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Slider setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Slider addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Slider removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public Slider setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }



  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();


    if (!this.callbacks.isEmpty()) {
      this.scrollEventSink = new SliderScrollEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.scrollEventSink.addCallback(this.callbacks.remove(0));
      }
    }


    if (Boolean.TRUE.equals(this.inverted)) {
      this.setInverted(this.inverted);
    }

    if (this.majorTickSpacing != 1) {
      this.setMajorTickSpacing(this.majorTickSpacing);
    }

    if (this.minorTickSpacing != 0) {
      this.setMinorTickSpacing(this.minorTickSpacing);
    }

    if (this.maximum != 100) {
      this.setMaximum(this.maximum);
    }

    if (this.minimum != 0) {
      this.setMinimum(this.minimum);
    }

    if (Boolean.TRUE.equals(this.paintLabels)) {
      this.setPaintLabels(this.paintLabels);
    }

    if (Boolean.TRUE.equals(this.paintTicks)) {
      this.setPaintTicks(this.paintLabels);
    }

    if (Boolean.TRUE.equals(this.snapToTicks)) {
      this.setPaintTicks(this.paintLabels);
    }

    if (this.value != 0) {
      this.setValue(this.value);
    }

    if (this.orientation != Orientation.HORIZONTAL) {
      this.setOrientation(Orientation.VERTICAL);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

    if (this.mouseWheelCondition != MouseWheelCondition.DEFAULT) {
      this.setScrollWheelBehavior(this.mouseWheelCondition);
    }


  }

}
