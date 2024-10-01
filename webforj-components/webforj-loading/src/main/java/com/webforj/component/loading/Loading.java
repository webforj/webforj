package com.webforj.component.loading;

import com.webforj.component.Component;
import com.webforj.component.Theme;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.component.spinner.DwcSpinner;
import com.webforj.component.spinner.SpinnerExpanse;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;

/**
 * An overlay that can be used to indicate activity while blocking user interaction. The loading
 * indicator appears on top of the parent's content, and can be dismissed by the API to resume user
 * interaction with the app.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@NodeName("dwc-loading")
public class Loading extends ElementCompositeContainer
    implements DwcLoading<Loading, Loading.LoadingSpinner>, HasClassName<Loading>,
    HasStyle<Loading>, HasText<Loading>, HasHtml<Loading> {

  // Properties
  @PropertyMethods(setter = "setText", getter = "getText")
  private final PropertyDescriptor<String> messageProp = PropertyDescriptor.property("message", "");

  @PropertyMethods(setter = "setBackdropVisible", getter = "isBackdropVisible")
  private final PropertyDescriptor<Boolean> noBackdropProp =
      PropertyDescriptor.property("noBackdrop", false);

  @PropertyMethods(setter = "setVisible", getter = "isVisible")
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setVisible", getter = "isVisible")
  private final PropertyDescriptor<Boolean> suppressSpinnerProp =
      PropertyDescriptor.property("suppressSpinner", false);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setClockwise", getter = "isClockwise")
  private final PropertyDescriptor<Boolean> spinnerClockwiseProp =
      PropertyDescriptor.property("spinnerClockwise", true);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setExpanse", getter = "getExpanse")
  private final PropertyDescriptor<SpinnerExpanse> spinnerExpanseProp =
      PropertyDescriptor.property("spinnerExpanse", SpinnerExpanse.NONE);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setPaused", getter = "isPaused")
  private final PropertyDescriptor<Boolean> spinnerPausedProp =
      PropertyDescriptor.property("spinnerPaused", false);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setTheme", getter = "getTheme")
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("spinnerTheme", Theme.DEFAULT);

  @PropertyMethods(target = LoadingSpinner.class, setter = "setSpeed", getter = "getSpeed")
  private final PropertyDescriptor<Integer> spinnerSpeedProp =
      PropertyDescriptor.property("spinnerSpeed", 1000);

  /**
   * Creates a new loading component with the given children.
   *
   * @param children The children of the loading component.
   */
  public Loading(Component... children) {
    super();
    add(children);
    setStyle("position", "absolute");
  }

  /**
   * Creates a new loading component with no children.
   *
   * @see #Loading(Component...)
   */
  public Loading() {
    this(new Component[0]);
  }

  /**
   * Creates a new loading component with the given text.
   *
   * @param text The text of the loading component.
   */
  public Loading(String text) {
    this();
    setText(text);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Loading setText(String text) {
    setHtml(sanitizeHtml(text));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return sanitizeHtml(getHtml());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Loading setHtml(String html) {
    set(messageProp, html);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return get(messageProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Loading setBackdropVisible(boolean backdropVisible) {
    set(noBackdropProp, !backdropVisible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isBackdropVisible() {
    return !get(noBackdropProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Loading setVisible(boolean visible) {
    set(openedProp, visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return get(openedProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DwcSpinner<LoadingSpinner> getSpinner() {
    return new LoadingSpinner();
  }

  private String sanitizeHtml(String html) {
    return html.replaceAll("\\<[^>]*>", "");
  }

  /**
   * The spinner instance of the loading component.
   */
  public final class LoadingSpinner implements DwcSpinner<LoadingSpinner> {

    /**
     * {@inheritDoc}
     */
    @Override
    public LoadingSpinner setClockwise(boolean clockwise) {
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
    public LoadingSpinner setExpanse(SpinnerExpanse expanse) {
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
    public LoadingSpinner setPaused(boolean paused) {
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
    public LoadingSpinner setSpeed(int speed) {
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
    public LoadingSpinner setTheme(Theme theme) {
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
    public LoadingSpinner setVisible(boolean visible) {
      set(suppressSpinnerProp, !visible);
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isVisible() {
      return !get(suppressSpinnerProp);
    }
  }

  Element getOriginalElement() {
    return getElement();
  }
}
