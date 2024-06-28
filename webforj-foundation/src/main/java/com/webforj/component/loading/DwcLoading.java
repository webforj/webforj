package com.webforj.component.loading;

import com.webforj.component.spinner.DwcSpinner;

/**
 * An interface that represents the DWC loading component.
 *
 * @param <T> the type of the component that implements this interface.
 * @param <S> the type of the spinner that the loading component uses.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface DwcLoading<T, S> {

  /**
   * Sets the message to be displayed in the loading component.
   *
   * @param message the message to be displayed in the loading component.
   * @return the component itself.
   */
  public T setText(String message);

  /**
   * Returns the message to be displayed in the loading component.
   *
   * @return the message to be displayed in the loading component.
   */
  public String getText();

  /**
   * Sets the HTML content to be displayed in the loading component.
   *
   * @param html the HTML content to be displayed in the loading component.
   * @return the component itself.
   */
  public T setHtml(String html);

  /**
   * Returns the HTML content to be displayed in the loading component.
   *
   * @return the HTML content to be displayed in the loading component.
   */
  public String getHtml();

  /**
   * Sets whether the backdrop is visible or invisible.
   *
   * @param backdropVisible true to make the backdrop visible, false to make it invisible.
   * @return the component itself.
   */
  public T setBackdropVisible(boolean backdropVisible);

  /**
   * Checks whether the backdrop is visible or invisible.
   *
   * @return true if the backdrop is visible, false if it's invisible.
   */
  public boolean isBackdropVisible();

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

  /**
   * Alias for {@link #setVisible(boolean)}.
   *
   * @see #setVisible(boolean)
   */
  public default T open() {
    return setVisible(true);
  }

  /**
   * Alias for {@link #setVisible(boolean)}.
   *
   * @see #setVisible(boolean)
   */
  public default T close() {
    return setVisible(false);
  }

  /**
   * Gets the loading's spinner.
   *
   * @return the loading's spinner.
   */
  public DwcSpinner<S> getSpinner();
}
