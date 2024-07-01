package com.webforj.component.spinner;

import com.webforj.component.Theme;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;

/**
 * A component to provide a way to show the progress of an indeterminate operation.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@NodeName("dwc-spinner")
public class Spinner extends ElementComposite implements DwcSpinner<Spinner>, HasClassName<Spinner>,
    HasStyle<Spinner>, HasText<Spinner>, HasHtml<Spinner>, HasVisibility<Spinner> {

  // Properties
  private final PropertyDescriptor<Boolean> spinnerClockwiseProp =
      PropertyDescriptor.property("clockwise", true);
  private final PropertyDescriptor<SpinnerExpanse> spinnerExpanseProp =
      PropertyDescriptor.property("expanse", SpinnerExpanse.NONE);
  private final PropertyDescriptor<Boolean> spinnerPausedProp =
      PropertyDescriptor.property("paused", false);
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.DEFAULT);
  private final PropertyDescriptor<Integer> spinnerSpeedProp =
      PropertyDescriptor.property("speed", 1000);

  /**
   * Creates a new instance of the spinner.
   *
   * @param theme the spinner theme
   * @param expanse the spinner expanse
   */
  public Spinner(Theme theme, SpinnerExpanse expanse) {
    super();
    setTheme(theme);
    setExpanse(expanse);
  }

  /**
   * Creates a new instance of the spinner.
   *
   * @param theme the spinner theme
   */
  public Spinner(Theme theme) {
    this(theme, SpinnerExpanse.NONE);
  }

  /**
   * Creates a new instance of the spinner.
   *
   * @param expanse the spinner expanse
   */
  public Spinner(SpinnerExpanse expanse) {
    this(Theme.DEFAULT, expanse);
  }

  /**
   * Creates a new instance of the spinner.
   */
  public Spinner() {
    this(Theme.DEFAULT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setClockwise(boolean clockwise) {
    set(spinnerClockwiseProp, clockwise);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isClockwise() {
    return get(spinnerClockwiseProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setExpanse(SpinnerExpanse expanse) {
    set(spinnerExpanseProp, expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SpinnerExpanse getExpanse() {
    return get(spinnerExpanseProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setPaused(boolean paused) {
    set(spinnerPausedProp, paused);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isPaused() {
    return get(spinnerPausedProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setSpeed(int speed) {
    set(spinnerSpeedProp, speed);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getSpeed() {
    return get(spinnerSpeedProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setTheme(Theme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Theme getTheme() {
    return get(themeProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Spinner setVisible(boolean visible) {
    getElement().setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return getElement().isVisible();
  }
}
