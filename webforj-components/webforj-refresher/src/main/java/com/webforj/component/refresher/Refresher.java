package com.webforj.component.refresher;

import com.webforj.component.Theme;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.refresher.event.RefresherRefreshEvent;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasTheme;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.utilities.Assets;

/**
 * A component that lets a user pull down on a container using touch in order to refresh or retrieve
 * more data.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@NodeName("dwc-refresher")
public class Refresher extends ElementCompositeContainer
    implements HasClassName<Refresher>, HasStyle<Refresher>, HasAttribute<Refresher>,
    HasEnablement<Refresher>, HasTheme<Refresher, Theme> {

  private RefresherI18n i18n = new RefresherI18n();

  @PropertyExclude
  private final PropertyDescriptor<Boolean> disabledProp =
      PropertyDescriptor.property("disabled", false);

  @PropertyMethods(setter = "setArrowIcon", getter = "getArrowIcon")
  private final PropertyDescriptor<String> iconArrowProp =
      PropertyDescriptor.property("iconArrow", "dwc:arrow-down");
  @PropertyMethods(setter = "setRefreshIcon", getter = "getRefreshIcon")
  private final PropertyDescriptor<String> iconRefreshProp =
      PropertyDescriptor.property("iconRefresh", "dwc:animated-spinner");

  private final PropertyDescriptor<Number> thresholdProp =
      PropertyDescriptor.property("threshold", 80);
  private final PropertyDescriptor<Number> thresholdMaxProp =
      PropertyDescriptor.property("thresholdMax", Double.POSITIVE_INFINITY);

  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.PRIMARY);

  @PropertyExclude
  private final PropertyDescriptor<String> textPullProp =
      PropertyDescriptor.property("textPull", "Pull down to refresh");
  @PropertyExclude
  private final PropertyDescriptor<String> textReleaseProp =
      PropertyDescriptor.property("textRelease", "Release to refresh");
  @PropertyExclude
  private final PropertyDescriptor<String> textRefreshProp =
      PropertyDescriptor.property("textRefresh", "Refreshing");

  /**
   * Sets the arrow icon to use when when the stage is {@code pull} or {@code release}.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   * @return the component itself
   */
  public Refresher setArrowIcon(String src) {
    set(iconArrowProp, Assets.resolveImageSource(src));
    return this;
  }

  /**
   * Sets the arrow icon to use when when the stage is {@code pull} or {@code release}.
   *
   * <p>
   * The method will only use the icon name and pool from the given icon. The icon component itself
   * will not be used.
   * </p>
   *
   * @param icon the icon
   * @return the component itself
   */
  public Refresher setArrowIcon(IconDefinition<?> icon) {
    set(iconArrowProp, String.format("%s:%s", icon.getPool(), icon.getName()));
    return this;
  }

  /**
   * Gets the arrow icon to use when when the stage is {@code pull} or {@code release}.
   *
   * @return the icon
   */
  public String getArrowIcon() {
    return get(iconArrowProp);
  }

  /**
   * Sets the refresh icon to use when the stage is {@code refreshing}.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   * @return the component itself
   */
  public Refresher setRefreshIcon(String src) {
    set(iconRefreshProp, Assets.resolveImageSource(src));
    return this;
  }

  /**
   * Sets the refresh icon to use when the stage is {@code refreshing}.
   *
   * <p>
   * The method will only use the icon name and pool from the given icon. The icon component itself
   * will not be used.
   * </p>
   *
   * @param icon the icon
   * @return the component itself
   */
  public Refresher setRefreshIcon(IconDefinition<?> icon) {
    set(iconRefreshProp, String.format("%s:%s", icon.getPool(), icon.getName()));
    return this;
  }

  /**
   * Gets the refresh icon to use when the stage is {@code refreshing}.
   *
   * @return the icon
   */
  public String getRefreshIcon() {
    return get(iconRefreshProp);
  }

  /**
   * Sets the refresher i18n object.
   *
   * @param i18n the refresher i18n object
   */
  public void setI18n(RefresherI18n i18n) {
    this.i18n = i18n;
    set(textPullProp, i18n.getPull());
    set(textReleaseProp, i18n.getRelease());
    set(textRefreshProp, i18n.getRefresh());
  }

  /**
   * Gets the refresher i18n object.
   *
   * @return the refresher i18n object
   */
  public RefresherI18n getI18n() {
    return i18n;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Refresher setEnabled(boolean enabled) {
    set(disabledProp, !enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return !get(disabledProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Refresher setTheme(Theme theme) {
    set(themeProp, theme.equals(Theme.DEFAULT) ? Theme.PRIMARY : theme);
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
   * Sets the minimum distance of the pull until the refresher will go into the release state.
   *
   * <p>
   * The threshold is defined as a number that represents a distance in pixels. It's used to
   * determine how far the user must pull down before the refresher enters the {@code release}
   * state. For example, if the pull distance is less than threshold, the stage remains in
   * {@code pull}. Once the pull distance exceeds the threshold, the stage switches to
   * {@code release}.
   * </p>
   *
   * @param threshold the minimum distance of the pull until the refresher will go into the release
   *        state
   * @return the component itself
   */
  public Refresher setThreshold(Number threshold) {
    set(thresholdProp, threshold);
    return this;
  }

  /**
   * Gets the minimum distance of the pull until the refresher will go into the release state.
   *
   * @return the minimum distance of the pull until the refresher will go into the release state
   * @see #setThreshold(int)
   */
  public Number getThreshold() {
    return get(thresholdProp);
  }

  /**
   * Sets the maximum distance of the pull.
   *
   * <p>
   * The threshold max represents the maximum pull distance, and its unit is pixels. By default,
   * it’s set to {@code Infinity}, which means there's no upper limit unless you specify one. This
   * setting ensures that even if the user pulls down further than desired, the pull distance (and
   * thus the component's height) is capped at the defined maximum value.
   * </p>
   *
   * @param thresholdMax the maximum distance of the pull
   * @return the component itself
   */
  public Refresher setThresholdMax(Number thresholdMax) {
    set(thresholdMaxProp, thresholdMax);
    return this;
  }

  /**
   * Gets the maximum distance of the pull.
   *
   * @return the maximum distance of the pull
   * @see #setThresholdMax(int)
   */
  public Number getThresholdMax() {
    return get(thresholdMaxProp);
  }

  /**
   * Resets the component to its idle or initial state once the refresh operation is complete.
   *
   * @return the component itself
   */
  public Refresher finish() {
    getElement().callJsFunctionVoidAsync("finish");
    return this;
  }

  /**
   * Adds a listener for the refresh event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RefresherRefreshEvent> addRefreshListener(
      EventListener<RefresherRefreshEvent> listener) {
    return addEventListener(RefresherRefreshEvent.class, listener);
  }

  /**
   * Alias for {@link #addRefreshListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RefresherRefreshEvent> onRefresh(
      EventListener<RefresherRefreshEvent> listener) {
    return addRefreshListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
