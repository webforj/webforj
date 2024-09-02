package com.webforj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.Component;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.button.sink.ButtonClickEventSink;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasPrefixAndSuffix;
import com.webforj.concern.HasTheme;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Arrays;
import java.util.List;

/**
 * An abstract DWC button component.
 *
 * <p>
 * This class represents an abstract button component in the DWC client. It extends the
 * {@link DwcFocusableComponent} class and implements several interfaces for handling properties
 * related to its appearance and behavior.
 * </p>
 *
 * @param <T> The type of the component.
 * @see DwcFocusableComponent
 * @see HasExpanse
 * @see HasTheme
 * @see HasHorizontalAlignment
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcButton<T extends DwcFocusableComponent<T>> extends DwcFocusableComponent<T>
    implements HasExpanse<T, Expanse>, HasTheme<T, ButtonTheme>, HasHorizontalAlignment<T>,
    HasFocusStatus, HasPrefixAndSuffix<T> {
  private static final String ICON_SLOT = "icon";
  private boolean disableOnClick = false;

  private final ComponentEventSinkRegistry<ButtonClickEvent> clickEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ButtonClickEventSink(this, getEventDispatcher()),
          ButtonClickEvent.class);

  protected DwcButton() {
    super();
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public boolean hasFocus() {
    return componentHasFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return super.<Expanse>getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setTheme(ButtonTheme theme) {
    setComponentTheme(theme);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ButtonTheme getTheme() {
    return super.<ButtonTheme>getComponentTheme();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * Sets whether the button should be immediately disabled when the user clicks it.
   *
   * @param disableOnClick {@code true} to disable the button when the user clicks it
   *
   * @return the component itself
   */
  public T setDisableOnClick(boolean disableOnClick) {
    BBjButton control = inferControl();

    if (control != null) {
      try {
        control.setDisableOnClick(disableOnClick);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.disableOnClick = disableOnClick;
    return getSelf();
  }

  /**
   * Returns whether the button is disabled when the user clicks it.
   *
   * @return {@code true} if the button is disabled when the user clicks it
   */
  public boolean isDisableOnClick() {
    BBjButton control = inferControl();

    if (control != null) {
      try {
        control.getDisableOnClick();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this.disableOnClick;
  }

  /**
   * Sets the button name which used for accessibility.
   *
   * <p>
   * The name is used for accessibility. When the name is not explicitly set, the label will be used
   * by the browser instead. This is useful when the label of the button is only an icon, for
   * instance.
   * </p>
   *
   * @param name the name of the button
   * @return the component itself
   */
  @Override
  public T setName(String name) {
    super.setName(name);
    setUnrestrictedAttribute("name", name);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setPrefixComponent(Component prefix) {
    super.setPrefixComponent(prefix);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Component getPrefixComponent() {
    return super.getPrefixComponent();
  }

  /**
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setSuffixComponent(Component suffix) {
    super.setSuffixComponent(suffix);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Component getSuffixComponent() {
    return super.getSuffixComponent();
  }

  /**
   * Sets the icon of the button.
   *
   * @param icon the icon of the button
   * @return the icon component
   */
  public Component setIcon(Component icon) {
    getSlotAssigner().reAssign(null, icon);
    return icon;
  }

  /**
   * Returns the icon of the button.
   *
   * @return the icon component
   */
  public Component getIcon() {
    return getSlotAssigner().getSlotComponent(null);
  }

  /**
   * Adds a {@link ButtonClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ButtonClickEvent> addClickListener(
      EventListener<ButtonClickEvent> listener) {
    return this.clickEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addClickListener(EventListener) addButtonClickListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<ButtonClickEvent> onClick(EventListener<ButtonClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autofocus", "disabled", "distance", "dropdownButtonWidth",
        "expanse", "hasFocus", "label", "name", "selected", "skidding", "tabTraversable", "theme",
        "toggleable", "value"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    clickEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (Boolean.TRUE.equals(disableOnClick)) {
      setDisableOnClick(disableOnClick);
    }
  }

  private BBjButton inferControl() {
    try {
      return (BBjButton) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
