package com.webforj.component.progressbar;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcComponent;
import com.webforj.component.Theme;
import com.webforj.component.window.Window;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasValue;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Represents a progress bar component.
 *
 * <p>
 * A progress bar is a visual component that shows the progress of a task, like downloading or
 * uploading a file. It usually appears as a horizontal bar that fills from left to right as the
 * task advances. Progress bars are also useful for tasks that aren't time-based, such as loading a
 * web page or processing a form. In these cases, the bar indicates the completion of a series of
 * steps needed to finish the task.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class ProgressBar extends DwcComponent<ProgressBar>
    implements HasValue<ProgressBar, Integer>, HasMin<ProgressBar, Integer>,
    HasMax<ProgressBar, Integer>, HasTheme<ProgressBar, Theme> {

  /**
   * Describes the orientation of the progress bar.
   */
  public enum Orientation {
    /**
     * The progress bar is rendered horizontally.
     */
    HORIZONTAL(0),
    /**
     * The progress bar is rendered vertically.
     */
    VERTICAL(1);

    private final int value;

    Orientation(int value) {
      this.value = value;
    }

    /**
     * Returns the value of the orientation.
     *
     * @return The value of the orientation.
     */
    public int getValue() {
      return value;
    }
  }

  private Integer value = 0;
  private Integer min = 0;
  private Integer max = 100;
  private Orientation orientation = Orientation.HORIZONTAL;
  private boolean indeterminate = false;
  private boolean textVisible = true;
  private boolean animated = false;
  private boolean striped = false;

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   * @param min The minimum value of the progress bar.
   * @param max The maximum value of the progress bar.
   * @param orientation The orientation of the progress bar.
   */
  public ProgressBar(Integer value, Integer min, Integer max, Orientation orientation) {
    super();

    setValue(value);
    setMin(min);
    setMax(max);
    setOrientation(orientation);
  }

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   * @param min The minimum value of the progress bar.
   * @param max The maximum value of the progress bar.
   */
  public ProgressBar(Integer value, Integer min, Integer max) {
    this(value, min, max, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   * @param max The maximum value of the progress bar.
   * @param orientation The orientation of the progress bar.
   */
  public ProgressBar(Integer value, Integer max, Orientation orientation) {
    this(value, 0, max, orientation);
  }

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   * @param max The maximum value of the progress bar.
   */
  public ProgressBar(Integer value, Integer max) {
    this(value, 0, max, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   * @param orientation The orientation of the progress bar.
   */
  public ProgressBar(Integer value, Orientation orientation) {
    this(value, 0, 100, orientation);
  }

  /**
   * Constructs a progress bar with the default values.
   *
   * @param value The value of the progress bar.
   */
  public ProgressBar(Integer value) {
    this(value, Orientation.HORIZONTAL);
  }

  /**
   * Constructs a progress bar with value and text.
   *
   * @param value The value of the progress bar.
   * @param text The text of the progress bar.
   */
  public ProgressBar(Integer value, String text) {
    this(value);
    setText(text);
  }

  /**
   * Constructs a progress bar with the default text.
   *
   * @param text The text of the progress bar.
   */
  public ProgressBar(String text) {
    this(0);
    setText(text);
  }

  /**
   * Constructs a progress bar with the default values.
   */
  public ProgressBar() {
    this(0);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ProgressBar setValue(Integer value) {
    Objects.requireNonNull(value, "Value cannot be null");
    this.value = value;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setValue(value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    getEventDispatcher().dispatchEvent(new ValueChangeEvent<>(this, value));

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer getValue() {
    return value;
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<Integer>> addValueChangeListener(
      EventListener<ValueChangeEvent<Integer>> listener) {
    return getEventDispatcher().addListener(ValueChangeEvent.class, listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ProgressBar setMin(Integer min) {
    Objects.requireNonNull(min, "Min cannot be null");

    this.min = min;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setMinimum(min);
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
  public ProgressBar setMax(Integer max) {
    Objects.requireNonNull(max, "Max cannot be null");

    this.max = max;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setMaximum(max);
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
   * Sets the progress bar orientation.
   *
   * @param orientation The orientation of the progress bar.
   * @return The component itself.
   */
  public ProgressBar setOrientation(Orientation orientation) {
    Objects.requireNonNull(orientation, "Orientation cannot be null");

    this.orientation = orientation;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setOrientation(orientation.getValue());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Returns the orientation of the progress bar.
   *
   * @return The orientation of the progress bar.
   */
  public Orientation getOrientation() {
    return orientation;
  }

  /**
   * Sets whether the progress bar is indeterminate.
   *
   * @param indeterminate {@code true} if the progress bar is indeterminate.
   * @return The component itself.
   */
  public ProgressBar setIndeterminate(boolean indeterminate) {
    this.indeterminate = indeterminate;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setIndeterminate(indeterminate);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Returns whether the progress bar is indeterminate.
   *
   * @return {@code true} if the progress bar is indeterminate.
   */
  public boolean isIndeterminate() {
    return indeterminate;
  }

  /**
   * Sets whether the text of the progress bar is visible.
   *
   * @param textVisible {@code true} if the text is visible.
   * @return The component itself.
   */
  public ProgressBar setTextVisible(boolean textVisible) {
    this.textVisible = textVisible;

    BBjProgressBar progressBar = inferControl();
    if (progressBar != null) {
      try {
        progressBar.setStringPainted(textVisible);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Returns whether the text of the progress bar is visible.
   *
   * @return {@code true} if the text is visible.
   */
  public boolean isTextVisible() {
    return textVisible;
  }

  /**
   * Sets whether the progress bar should be animated.
   *
   * @param animated {@code true} if the progress bar is animated.
   * @return The component itself.
   */
  public ProgressBar setAnimated(boolean animated) {
    this.animated = animated;
    setUnrestrictedAttribute("animated", String.valueOf(animated));
    return this;
  }

  /**
   * Returns whether the progress bar is animated.
   *
   * @return {@code true} if the progress bar is animated.
   */
  public boolean isAnimated() {
    return animated;
  }

  /**
   * Sets whether the progress bar should be striped.
   *
   * @param striped {@code true} if the progress bar is striped.
   * @return The component itself.
   */
  public ProgressBar setStriped(boolean striped) {
    this.striped = striped;
    setUnrestrictedAttribute("striped", String.valueOf(striped));
    return this;
  }

  /**
   * Returns whether the progress bar is striped.
   *
   * @return {@code true} if the progress bar is striped.
   */
  public boolean isStriped() {
    return striped;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ProgressBar setTheme(Theme theme) {
    setComponentTheme(theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Theme getTheme() {
    return super.<Theme>getComponentTheme();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {

    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("animated", "disabled", "indeterminate", "max", "min",
        "orientation", "striped", "text", "text-visible", "theme", "value"));

    return properties;
  }

  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      setControl(w.addProgressBar());
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjProgressBar Control", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (max != 100) {
      setMax(max);
    }

    if (min != 0) {
      setMin(min);
    }

    if (value != 0) {
      setValue(value);
    }

    if (!orientation.equals(Orientation.HORIZONTAL)) {
      setOrientation(orientation);
    }

    if (indeterminate) {
      setIndeterminate(indeterminate);
    }

    if (!textVisible) {
      setTextVisible(textVisible);
    }
  }

  private BBjProgressBar inferControl() {
    try {
      return (BBjProgressBar) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
