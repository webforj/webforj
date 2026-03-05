package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.element.annotation.NodeName;

/**
 * A renderer that displays a {@code dwc-progressbar} in a table cell.
 *
 * <p>
 * When {@code value} is not explicitly set, it defaults to the column's value function result, so
 * the column should return a numeric value.
 * </p>
 *
 * <pre>{@code
 * ProgressBarRenderer<MusicRecord> renderer = new ProgressBarRenderer<>();
 * renderer.setMax(20);
 * renderer.setTheme(Theme.INFO);
 * renderer.setTextVisible(true);
 *
 * table.addColumn("cost", MusicRecord::getCost).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-progressbar")
public class ProgressBarRenderer<T> extends AbstractVoidElementRenderer<T> {
  private static final String VALUE_ATTR = "value";
  private static final String STYLE_ATTR = "style";

  private Integer value;
  private Integer min;
  private Integer max;
  private boolean indeterminate = false;
  private boolean textVisible = false;
  private boolean animated = false;
  private boolean striped = false;
  private Theme theme;
  private String text;

  /**
   * Creates a new progress bar renderer with the given value, min and max.
   *
   * @param value the progress value
   * @param min the minimum value
   * @param max the maximum value
   */
  public ProgressBarRenderer(Integer value, Integer min, Integer max) {
    this(value, max);
    setMin(min);
  }

  /**
   * Creates a new progress bar renderer with the given value and max.
   *
   * @param value the progress value
   * @param max the maximum value
   */
  public ProgressBarRenderer(Integer value, Integer max) {
    this(value);
    setMax(max);
  }

  /**
   * Creates a new progress bar renderer with the given value.
   *
   * @param value the progress value
   */
  public ProgressBarRenderer(Integer value) {
    this();
    setValue(value);
  }

  /**
   * Creates a new progress bar renderer.
   */
  public ProgressBarRenderer() {
    setContent("");
  }

  /**
   * Sets the value of the progress bar.
   *
   * <p>
   * When not called, the value is bound to the cell value.
   * </p>
   *
   * @param value the progress value
   * @return this renderer
   */
  public ProgressBarRenderer<T> setValue(Integer value) {
    this.value = value;
    setAttribute(VALUE_ATTR, String.valueOf(value));
    return this;
  }

  /**
   * Returns the value of the progress bar.
   *
   * @return the value
   */
  public Integer getValue() {
    return value;
  }

  /**
   * Sets the minimum value of the progress bar.
   *
   * @param min the minimum value
   * @return this renderer
   */
  public ProgressBarRenderer<T> setMin(Integer min) {
    this.min = min;
    setAttribute("min", String.valueOf(min));
    return this;
  }

  /**
   * Returns the minimum value of the progress bar.
   *
   * @return the minimum value
   */
  public Integer getMin() {
    return min;
  }

  /**
   * Sets the maximum value of the progress bar.
   *
   * @param max the maximum value
   * @return this renderer
   */
  public ProgressBarRenderer<T> setMax(Integer max) {
    this.max = max;
    setAttribute("max", String.valueOf(max));
    return this;
  }

  /**
   * Returns the maximum value of the progress bar.
   *
   * @return the maximum value
   */
  public Integer getMax() {
    return max;
  }

  /**
   * Sets whether the progress bar is indeterminate.
   *
   * @param indeterminate {@code true} if the progress bar is indeterminate
   * @return this renderer
   */
  public ProgressBarRenderer<T> setIndeterminate(boolean indeterminate) {
    this.indeterminate = indeterminate;
    setAttribute("indeterminate", String.valueOf(indeterminate));
    return this;
  }

  /**
   * Returns whether the progress bar is indeterminate.
   *
   * @return {@code true} if the progress bar is indeterminate
   */
  public boolean isIndeterminate() {
    return indeterminate;
  }

  /**
   * Sets whether the text of the progress bar is visible.
   *
   * @param textVisible {@code true} if the text is visible
   * @return this renderer
   */
  public ProgressBarRenderer<T> setTextVisible(boolean textVisible) {
    this.textVisible = textVisible;
    setAttribute("text-visible", String.valueOf(textVisible));
    return this;
  }

  /**
   * Returns whether the text of the progress bar is visible.
   *
   * @return {@code true} if the text is visible
   */
  public boolean isTextVisible() {
    return textVisible;
  }

  /**
   * Sets whether the progress bar should be animated.
   *
   * @param animated {@code true} if the progress bar is animated
   * @return this renderer
   */
  public ProgressBarRenderer<T> setAnimated(boolean animated) {
    this.animated = animated;
    setAttribute("animated", String.valueOf(animated));
    return this;
  }

  /**
   * Returns whether the progress bar is animated.
   *
   * @return {@code true} if the progress bar is animated
   */
  public boolean isAnimated() {
    return animated;
  }

  /**
   * Sets whether the progress bar should be striped.
   *
   * @param striped {@code true} if the progress bar is striped
   * @return this renderer
   */
  public ProgressBarRenderer<T> setStriped(boolean striped) {
    this.striped = striped;
    setAttribute("striped", String.valueOf(striped));
    return this;
  }

  /**
   * Returns whether the progress bar is striped.
   *
   * @return {@code true} if the progress bar is striped
   */
  public boolean isStriped() {
    return striped;
  }

  /**
   * Sets the theme of the progress bar.
   *
   * @param theme the theme to set
   * @return this renderer
   */
  public ProgressBarRenderer<T> setTheme(Theme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Returns the theme of the progress bar.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return theme;
  }

  /**
   * Sets the text displayed on the progress bar.
   *
   * <p>
   * Supports {@code {{x}}} placeholder for percentage and {@code {{value}}} for the raw value.
   * </p>
   *
   * @param text the text to display
   * @return this renderer
   */
  public ProgressBarRenderer<T> setText(String text) {
    this.text = text;
    setAttribute("text", text);
    return this;
  }

  /**
   * Returns the text displayed on the progress bar.
   *
   * @return the text
   */
  public String getText() {
    return text;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    if (value == null) {
      setAttribute(VALUE_ATTR, "<%= cell.value %>", false);
    }
    setAttribute(STYLE_ATTR, "--dwc-progressbar-height:15px;--dwc-font-size-s:0.8rem", false);
    return super.build();
  }
}
