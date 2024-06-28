package com.webforj.component.spinner;

import com.webforj.component.Theme;

/**
 * An interface that represents the DWC spinner component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface DwcSpinner<T> {

  /**
   * Sets whether the animation will be played clockwise or counter clockwise.
   *
   * @param clockwise When true, the animation will be played clockwise, counter clockwise
   *        otherwise.
   * @return the component itself
   */
  public T setClockwise(boolean clockwise);

  /**
   * Returns whether the animation will be played clockwise or counter clockwise.
   *
   * @return true if the animation will be played clockwise, false otherwise
   */
  public boolean isClockwise();

  /**
   * Sets the component's expanse.
   *
   * @param expanse The component's expanse
   * @return the component itself
   */
  public T setExpanse(SpinnerExpanse expanse);

  /**
   * Returns the component's expanse.
   *
   * @return the component's expanse
   */
  public SpinnerExpanse getExpanse();

  /**
   * Sets whether the animation will be paused or played.
   *
   * @param paused When true, the animation will be paused, played otherwise.
   * @return the component itself
   */
  public T setPaused(boolean paused);

  /**
   * Returns whether the animation will be paused or played.
   *
   * @return true if the animation will be paused, false otherwise
   */
  public boolean isPaused();

  /**
   * Sets the animation speed in milliseconds.
   *
   * <p>
   * The speed is the time it takes for the animation to complete one full cycle. The default speed
   * is 1000 milliseconds. The lower the number the faster the animation.
   * </p>
   *
   * @param speed The animation speed in milliseconds
   * @return the component itself
   */
  public T setSpeed(int speed);

  /**
   * Returns the animation speed in milliseconds.
   *
   * @return the animation speed in milliseconds
   */
  public int getSpeed();

  /**
   * Sets the spinner theme.
   *
   * @param theme The spinner theme
   * @return the component itself
   */
  public T setTheme(Theme theme);

  /**
   * Returns the spinner theme.
   *
   * @return the spinner theme
   */
  public Theme getTheme();

  /**
   * Sets whether the spinner is visible or invisible.
   *
   * @param visible true to make the spinner visible, false to make it invisible.
   * @return the component itself.
   */
  public T setVisible(boolean visible);

  /**
   * Checks whether the spinner is visible or invisible.
   *
   * @return true if the spinner is visible, false if it's invisible.
   */
  public boolean isVisible();
}
