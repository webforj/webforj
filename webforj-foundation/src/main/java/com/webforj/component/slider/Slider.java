package com.webforj.component.slider;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.google.gson.annotations.SerializedName;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.Theme;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.slider.event.SliderSlideEvent;
import com.webforj.component.slider.sink.SliderSlideEventSink;
import com.webforj.component.window.Window;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasValue;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A component that lets the user graphically select a value by sliding a knob within a bounded
 * interval. The knob is always positioned at the points that match integer values within the
 * specified interval.
 *
 * @since 24.10
 */
public final class Slider extends DwcFocusableComponent<Slider> implements HasMin<Slider, Integer>,
    HasMax<Slider, Integer>, HasValue<Slider, Integer>, HasTheme<Slider, Theme> {

  static final String PROP_ORIENTATION = "orientation";
  static final String PROP_SLIDE_BY_WHEEL = "slidableByWheel";
  static final String PROP_SLIDE_BY_CLICKING_TICKS = "slideByClickingPips";
  static final String PROP_TOOLTIP_VISIBLE = "tooltips";
  static final String PROP_TOOLTIP_VISIBLE_ON_SLIDE_ONLY = "tooltipsOnlyWhenSliding";
  static final String PROP_TOOLTIP = "tooltipExpression";
  static final String PROP_FILLED = "connect";

  private final ComponentEventSinkRegistry<SliderSlideEvent> slideEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new SliderSlideEventSink(this, getEventDispatcher()),
          SliderSlideEvent.class);

  private Integer min = 0;
  private Integer max = 100;
  private int majorTickSpacing = 0;
  private int minorTickSpacing = 0;
  private boolean snapToTicks = false;
  private Orientation orientation = Orientation.HORIZONTAL;
  private Integer value = 50;
  private boolean ticksVisible = false;
  private boolean inverted = false;
  private Map<Integer, String> labels = null;
  private boolean labelsVisible = false;
  private boolean allowMajorLabelsOverlap = false;
  private boolean registeredSlideValueChangeListener = false;
  private boolean slideByWheel = true;
  private boolean slideByClickingTicks = true;
  private String tooltipText = "";
  private boolean tooltipVisible = false;
  private boolean tooltipVisibleOnSlideOnly = false;
  private boolean filled = false;

  /**
   * Describes the orientation of the slider.
   */
  public enum Orientation {
    /**
     * The slider is rendered horizontally.
     */
    @SerializedName("horizontal")
    HORIZONTAL,

    /**
     * The slider is rendered vertically.
     */
    @SerializedName("vertical")
    VERTICAL
  }

  /**
   * Constructs a slider with the specified value, minimum value, maximum value, and orientation.
   *
   * @param value The value of the slider.
   * @param min The minimum value of the slider.
   * @param max The maximum value of the slider.
   * @param orientation The orientation of the slider.
   */
  public Slider(Integer value, Integer min, Integer max, Orientation orientation) {
    super();
    setValue(value);
    setMin(min);
    setMax(max);
    setOrientation(orientation);
  }

  /**
   * Constructs a slider with the specified value, minimum value, and maximum value.
   *
   * @param value The value of the slider.
   * @param min The minimum value of the slider.
   * @param max The maximum value of the slider.
   */
  public Slider(Integer value, Integer min, Integer max) {
    this(value, min, max, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a slider with the specified value, maximum value, and orientation.
   *
   * @param value The value of the slider.
   * @param max The maximum value of the slider.
   * @param orientation The orientation of the slider.
   */
  public Slider(Integer value, Integer max, Orientation orientation) {
    this(value, 0, max, orientation);
  }

  /**
   * Constructs a slider with the specified value and maximum value.
   *
   * @param value The value of the slider.
   * @param max The maximum value of the slider.
   */
  public Slider(Integer value, Integer max) {
    this(value, 0, max, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a slider with the specified value and orientation.
   *
   * @param value The value of the slider.
   * @param orientation The orientation of the slider.
   */
  public Slider(Integer value, Orientation orientation) {
    this(value, 0, 100, orientation);
  }

  /**
   * Constructs a slider with the specified value.
   *
   * @param value The value of the slider.
   */
  public Slider(Integer value) {
    this(value, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a slider with the default values.
   */
  public Slider() {
    this(0);
  }

  /**
   * Sets the orientation of the slider.
   *
   * @param orientation the orientation of the slider
   * @return the component itself
   */
  public Slider setOrientation(Orientation orientation) {
    Objects.requireNonNull(orientation, "The orientation cannot be null");
    this.orientation = orientation;
    setUnrestrictedProperty(PROP_ORIENTATION, orientation);
    return this;
  }

  /**
   * Gets the orientation of the slider.
   *
   * @return the orientation of the slider
   */
  public Orientation getOrientation() {
    return orientation;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Slider setValue(Integer value) {
    Objects.requireNonNull(value, "The value cannot be null");
    this.value = value;

    BBjSlider slider = inferSlider();
    if (slider != null) {
      try {
        slider.setValue(value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer getValue() {
    return value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Slider setMin(Integer min) {
    Objects.requireNonNull(min, "The minimum value cannot be null");
    this.min = min;

    BBjSlider slider = inferSlider();
    if (slider != null) {
      try {
        slider.setMinimum(min);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Slider setMax(Integer max) {
    Objects.requireNonNull(max, "The maximum value cannot be null");
    this.max = max;

    BBjSlider slider = inferSlider();
    if (slider != null) {
      try {
        slider.setMaximum(max);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer getMax() {
    return max;
  }

  /**
   * Sets the major tick spacing.
   *
   * <p>
   * The number of values between the major tick marks -- the larger marks that break up the minor
   * tick marks.
   * </p>
   *
   * @param majorTickSpacing the major tick spacing
   * @return the component itself
   */
  public Slider setMajorTickSpacing(int majorTickSpacing) {
    this.majorTickSpacing = majorTickSpacing;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setMajorTickSpacing(majorTickSpacing);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the major tick spacing.
   *
   * @return the major tick spacing
   */
  public int getMajorTickSpacing() {
    return majorTickSpacing;
  }

  /**
   * Sets the minor tick spacing.
   *
   * <p>
   * The number of values between the minor tick marks -- the smaller marks that occur between the
   * major tick marks.
   * </p>
   *
   * @param minorTickSpacing the minor tick spacing
   * @return the component itself
   */
  public Slider setMinorTickSpacing(int minorTickSpacing) {
    this.minorTickSpacing = minorTickSpacing;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setMinorTickSpacing(minorTickSpacing);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the minor tick spacing.
   *
   * @return the minor tick spacing
   */
  public int getMinorTickSpacing() {
    return minorTickSpacing;
  }

  /**
   * Sets whether the slider should snap to the nearest tick mark.
   *
   * @param snapToTicks {@code true} if the slider should snap to the nearest tick mark,
   *        {@code false} otherwise
   * @return the component itself
   */
  public Slider setSnapToTicks(boolean snapToTicks) {
    this.snapToTicks = snapToTicks;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setSnapToTicks(snapToTicks);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the slider should snap to the nearest tick mark.
   *
   * @return {@code true} if the slider should snap to the nearest tick mark, {@code false}
   *         otherwise
   */
  public boolean isSnapToTicks() {
    return snapToTicks;
  }

  /**
   * Sets whether the slider should show tick marks.
   *
   * @param ticksVisible {@code true} if the slider should show tick marks, {@code false} otherwise
   * @return the component itself
   */
  public Slider setTicksVisible(boolean ticksVisible) {
    this.ticksVisible = ticksVisible;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setPaintTicks(ticksVisible);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the slider should show tick marks.
   *
   * @return {@code true} if the slider should show tick marks, {@code false} otherwise
   */
  public boolean isTicksVisible() {
    return ticksVisible;
  }

  /**
   * Sets whether the slider should be inverted.
   *
   * <p>
   * By default, the minimum value of a vertical slider is at the bottom and the maximum value is at
   * the top. For a horizontal slider, the minimum value is to the left and the maximum value is to
   * the right. The orientation reverses for inverted sliders.
   * </p>
   *
   * @param inverted {@code true} if the slider should be inverted, {@code false} otherwise
   * @return the component itself
   */
  public Slider setInverted(boolean inverted) {
    this.inverted = inverted;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setInverted(inverted);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the slider should be inverted.
   *
   * @return {@code true} if the slider should be inverted, {@code false} otherwise
   */
  public boolean isInverted() {
    return inverted;
  }

  /**
   * Sets the labels for the tick marks.
   *
   * <p>
   * The labels are displayed at the tick marks. The keys are the values of the tick marks and the
   * values are the labels to be displayed.
   * </p>
   *
   * @param labels the labels for the tick marks
   * @return the component itself
   */
  public Slider setLabels(Map<Integer, String> labels) {
    this.labels = labels;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setLabels(labels);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the labels for the tick marks.
   *
   * @return the labels for the tick marks
   */
  public Map<Integer, String> getLabels() {
    Map<Integer, String> result = new LinkedHashMap<>();
    boolean hasCustomLabels = labels != null && !labels.isEmpty();

    if (allowMajorLabelsOverlap || !hasCustomLabels) {
      int minValue = this.min;
      int maxValue = this.max;
      int majorTickSpacingValue = this.majorTickSpacing;

      if (majorTickSpacingValue > 0) {
        for (int index = minValue; index <= maxValue; index += majorTickSpacingValue) {
          result.put(index, String.valueOf(index));
        }
      }
    }

    if (hasCustomLabels) {
      for (Map.Entry<Integer, String> entry : labels.entrySet()) {
        result.put(entry.getKey(), entry.getValue());
      }
    }

    return result;
  }

  /**
   * Sets whether the slider should show labels for the tick marks.
   *
   * @param labelsVisible {@code true} if the slider should show labels for the tick marks,
   *        {@code false} otherwise
   * @return the component itself
   */
  public Slider setLabelsVisible(boolean labelsVisible) {
    this.labelsVisible = labelsVisible;
    BBjSlider slider = inferSlider();

    if (slider != null) {
      try {
        slider.setPaintLabels(labelsVisible);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the slider should show labels for the tick marks.
   *
   * @return {@code true} if the slider should show labels for the tick marks, {@code false}
   *         otherwise
   */
  public boolean isLabelsVisible() {
    return labelsVisible;
  }

  /**
   * Sets whether the major custom labels and auto-generated labels are allowed to overlap.
   *
   * <p>
   * When true, the slider will paint major ticks labels and the custom labels at the same time.
   * </p>
   *
   * @param allowMajorLabelsOverlap {@code true} if the major custom labels and auto-generated
   *        labels are allowed to overlap, {@code false} otherwise
   * @return the component itself
   */
  public Slider setAllowMajorLabelsOverlap(boolean allowMajorLabelsOverlap) {
    this.allowMajorLabelsOverlap = allowMajorLabelsOverlap;
    setUnrestrictedProperty("allowMajorLabelsOverlap", allowMajorLabelsOverlap);

    return this;
  }

  /**
   * Gets whether the major custom labels and auto-generated labels are allowed to overlap.
   *
   * @return {@code true} if the major custom labels and auto-generated labels are allowed to
   *         overlap, {@code false} otherwise
   */
  public boolean isAllowMajorLabelsOverlap() {
    return allowMajorLabelsOverlap;
  }

  /**
   * Sets whether the slider should be slidable by the mouse wheel.
   *
   * @param slideByWheel {@code true} if the slider should be slidable by the mouse wheel,
   *        {@code false} otherwise
   * @return the component itself
   */
  public Slider setSlideByWheel(boolean slideByWheel) {
    this.slideByWheel = slideByWheel;
    setUnrestrictedProperty(PROP_SLIDE_BY_WHEEL, slideByWheel);

    return this;
  }

  /**
   * Gets whether the slider should be slidable by the mouse wheel.
   *
   * @return {@code true} if the slider should be slidable by the mouse wheel, {@code false}
   *         otherwise
   */
  public boolean isSlideByWheel() {
    return slideByWheel;
  }

  /**
   * Sets whether the slider should be slidable by clicking on the ticks.
   *
   * @param slideByClickingTicks {@code true} if the slider should be slidable by clicking on the
   *        ticks, {@code false} otherwise
   * @return the component itself
   */
  public Slider setSlideByClickingTicks(boolean slideByClickingTicks) {
    this.slideByClickingTicks = slideByClickingTicks;
    setUnrestrictedProperty(PROP_SLIDE_BY_CLICKING_TICKS, slideByClickingTicks);

    return this;
  }

  /**
   * Gets whether the slider should be slidable by clicking on the ticks.
   *
   * @return {@code true} if the slider should be slidable by clicking on the ticks, {@code false}
   *         otherwise
   */
  public boolean isSlideByClickingTicks() {
    return slideByClickingTicks;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Slider setTheme(Theme theme) {
    super.setComponentTheme(theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Theme getTheme() {
    return super.getComponentTheme();
  }

  /**
   * Sets the tooltip text.
   *
   * <p>
   * The tooltip can be a JavaScript expression to format the tooltip. If the expression has the
   * 'return' keyword in it, then it is used as is; otherwise, it is wrapped with 'return' and ';'
   * to make a function. For example, "return x + '$'".
   * </p>
   *
   * @param text the tooltip text
   * @return the component itself
   */
  @Override
  public Slider setTooltipText(String text) {
    Objects.requireNonNull(text, "The tooltip text cannot be null");
    this.tooltipText = text;
    setUnrestrictedProperty(PROP_TOOLTIP, text);

    if (!text.trim().isEmpty() && !isTooltipVisible()) {
      setTooltipVisible(true);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getTooltipText() {
    return tooltipText;
  }

  /**
   * Sets whether the tooltip should be visible.
   *
   * @param tooltipVisible {@code true} if the tooltip should be visible, {@code false} otherwise
   * @return the component itself
   */
  public Slider setTooltipVisible(boolean tooltipVisible) {
    this.tooltipVisible = tooltipVisible;
    setUnrestrictedProperty(PROP_TOOLTIP_VISIBLE, tooltipVisible);

    return this;
  }

  /**
   * Gets whether the tooltip should be visible.
   *
   * @return {@code true} if the tooltip should be visible, {@code false} otherwise
   */
  public boolean isTooltipVisible() {
    return tooltipVisible;
  }

  /**
   * Sets whether the tooltip should be visible only when sliding.
   *
   * @param tooltipVisibleOnSlideOnly {@code true} if the tooltip should be visible only when
   *        sliding, {@code false} otherwise
   * @return the component itself
   */
  public Slider setTooltipVisibleOnSlideOnly(boolean tooltipVisibleOnSlideOnly) {
    if (tooltipVisibleOnSlideOnly && !isTooltipVisible()) {
      setTooltipVisible(true);
    }

    this.tooltipVisibleOnSlideOnly = tooltipVisibleOnSlideOnly;
    setUnrestrictedProperty(PROP_TOOLTIP_VISIBLE_ON_SLIDE_ONLY, tooltipVisibleOnSlideOnly);

    return this;
  }

  /**
   * Gets whether the tooltip should be visible only when sliding.
   *
   * @return {@code true} if the tooltip should be visible only when sliding, {@code false}
   *         otherwise
   */
  public boolean isTooltipVisibleOnSlideOnly() {
    return tooltipVisibleOnSlideOnly;
  }

  /**
   * Sets whether the knob will be connected to the slider edge.
   *
   * @param filled {@code true} if the knob will be connected to the slider edge, {@code false}
   *        otherwise
   * @return the component itself
   */
  public Slider setFilled(boolean filled) {
    this.filled = filled;
    setUnrestrictedProperty(PROP_FILLED, filled);
    return this;
  }

  /**
   * Gets whether the knob will be connected to the slider edge.
   *
   * @return {@code true} if the knob will be connected to the slider edge, {@code false} otherwise
   */
  public boolean isFilled() {
    return filled;
  }

  /**
   * Adds a {@link SliderSlideEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<SliderSlideEvent> addSlideEvent(
      EventListener<SliderSlideEvent> listener) {
    return this.slideEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addSlideEvent(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<SliderSlideEvent> onSlide(EventListener<SliderSlideEvent> listener) {
    return addSlideEvent(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<ValueChangeEvent<Integer>> addValueChangeListener(
      EventListener<ValueChangeEvent<Integer>> listener) {
    ListenerRegistration<ValueChangeEvent<Integer>> registration =
        getEventDispatcher().addListener(ValueChangeEvent.class, listener);

    if (!registeredSlideValueChangeListener) {
      addSlideEvent((ev -> {
        if (ev.isAdjusting()) {
          return;
        }

        ValueChangeEvent<Integer> valueChangeEvent = new ValueChangeEvent<>(this, ev.getValue());
        getEventDispatcher().dispatchEvent(valueChangeEvent);
      }));

      registeredSlideValueChangeListener = true;
    }

    return registration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    slideEventSinkListenerRegistry.attach();
  }

  @Override
  protected void onAttach() {
    super.onAttach();

    if (min != 0) {
      setMin(min);
    }

    if (max != 100) {
      setMax(max);
    }

    if (majorTickSpacing != 0) {
      setMajorTickSpacing(majorTickSpacing);
    }

    if (minorTickSpacing != 0) {
      setMinorTickSpacing(minorTickSpacing);
    }

    if (snapToTicks) {
      setSnapToTicks(snapToTicks);
    }

    if (value != 0) {
      setValue(value);
    }

    if (ticksVisible) {
      setTicksVisible(ticksVisible);
    }

    if (inverted) {
      setInverted(inverted);
    }

    if (labels != null && !labels.isEmpty()) {
      setLabels(labels);
    }

    if (labelsVisible) {
      setLabelsVisible(labelsVisible);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addHorizontalSlider(flags));
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjSlider Control", e);
    }
  }

  private BBjSlider inferSlider() {
    try {
      return (BBjSlider) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
