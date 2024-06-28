package com.webforj;

import com.basis.bbj.proxies.BBjBusyIndicator;
import com.basis.startup.type.BBjException;
import com.google.gson.Gson;
import com.webforj.component.Theme;
import com.webforj.component.loading.DwcLoading;
import com.webforj.component.spinner.DwcSpinner;
import com.webforj.component.spinner.SpinnerExpanse;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * A busy indicator is a visual element that signals the application is loading or processing data.
 *
 * <p>
 * It can display a loading spinner and a message to the user. The spinner is customizable with
 * different themes, speeds, and directions. The message can be set as plain text or HTML.
 * </p>
 *
 * @version 24.10
 */
public final class BusyIndicator
    implements DwcLoading<BusyIndicator, BusyIndicator.BusyIndicatorSpinner> {

  private static final String ATTR_DATA_BUSY_INDICATOR = "data-busy-indicator";
  private static final String ATTR_NO_BACKDROP = "no-backdrop";
  private static final String ATTR_SUPPRESS_SPINNER = "suppress-spinner";
  private static final String ATTR_SPINNER_CLOCKWISE = "spinner-clockwise";
  private static final String ATTR_SPINNER_EXPANSE = "spinner-expanse";
  private static final String ATTR_SPINNER_PAUSED = "spinner-paused";
  private static final String ATTR_SPINNER_THEME = "spinner-theme";
  private static final String ATTR_SPINNER_SPEED = "spinner-speed";
  private static final String FALSE = "false";
  private static final String TRUE = "true";


  private final BBjBusyIndicator indicator;

  BusyIndicator(BBjBusyIndicator indicator) {
    this.indicator = indicator;

    // set all default values
    setAttribute(ATTR_DATA_BUSY_INDICATOR, "");
    setAttribute(ATTR_NO_BACKDROP, FALSE);
    setAttribute(ATTR_SUPPRESS_SPINNER, FALSE);
    setAttribute(ATTR_SPINNER_CLOCKWISE, TRUE);
    setAttribute(ATTR_SPINNER_EXPANSE, "");
    setAttribute(ATTR_SPINNER_PAUSED, FALSE);
    setAttribute(ATTR_SPINNER_THEME, "default");
    setAttribute(ATTR_SPINNER_SPEED, "1000");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BusyIndicator setText(String message) {
    indicator.setText(message);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return indicator.getText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BusyIndicator setHtml(String html) {
    indicator.setHtml(html);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return indicator.getHtml();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BusyIndicator setBackdropVisible(boolean backdropVisible) {
    setAttribute(ATTR_NO_BACKDROP, backdropVisible ? FALSE : TRUE);
    return this;
  }

  @Override
  public boolean isBackdropVisible() {
    String attribute = getAttribute(ATTR_NO_BACKDROP);
    return attribute == null || attribute.equals(FALSE);
  }

  @Override
  public DwcSpinner<BusyIndicatorSpinner> getSpinner() {
    return new BusyIndicatorSpinner();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BusyIndicator setVisible(boolean visible) {
    indicator.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return indicator.isVisible();
  }

  void setAttribute(String name, String value) {
    try {
      indicator.setAttribute(name, value);
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  String getAttribute(String name) {
    try {
      return indicator.getAttribute(name);
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  /**
   * Represents the spinner of the busy indicator.
   */
  public final class BusyIndicatorSpinner implements DwcSpinner<BusyIndicatorSpinner> {

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setVisible(boolean visible) {
      setAttribute(ATTR_SUPPRESS_SPINNER, visible ? FALSE : TRUE);
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isVisible() {
      String attribute = getAttribute(ATTR_SUPPRESS_SPINNER);
      return attribute == null || attribute.equals(FALSE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setClockwise(boolean clockwise) {
      setAttribute(ATTR_SPINNER_CLOCKWISE, clockwise ? TRUE : FALSE);
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isClockwise() {
      String attribute = getAttribute(ATTR_SPINNER_CLOCKWISE);
      return attribute == null || attribute.equals(TRUE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setExpanse(SpinnerExpanse expanse) {
      Gson gson = new Gson();
      String value = gson.fromJson(gson.toJson(expanse), String.class);
      setAttribute(ATTR_SPINNER_EXPANSE, value == null ? "" : value);
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SpinnerExpanse getExpanse() {
      String attribute = getAttribute(ATTR_SPINNER_EXPANSE);
      if (attribute == null || (attribute != null && attribute.isEmpty())) {
        return SpinnerExpanse.NONE;
      }

      Gson gson = new Gson();
      return gson.fromJson(attribute, SpinnerExpanse.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setPaused(boolean paused) {
      setAttribute(ATTR_SPINNER_PAUSED, paused ? TRUE : FALSE);
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isPaused() {
      String attribute = getAttribute(ATTR_SPINNER_PAUSED);
      return attribute != null && attribute.equals(TRUE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setSpeed(int speed) {
      setAttribute(ATTR_SPINNER_SPEED, String.valueOf(speed));
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSpeed() {
      String attribute = getAttribute(ATTR_SPINNER_SPEED);
      return attribute == null ? 0 : Integer.parseInt(attribute);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BusyIndicatorSpinner setTheme(Theme theme) {
      Gson gson = new Gson();
      setAttribute(ATTR_SPINNER_THEME, gson.fromJson(gson.toJson(theme), String.class));
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Theme getTheme() {
      String attribute = getAttribute(ATTR_SPINNER_THEME);
      if (attribute == null || (attribute != null && attribute.isEmpty())) {
        return Theme.DEFAULT;
      }

      Gson gson = new Gson();
      return gson.fromJson(attribute, Theme.class);
    }
  }
}
