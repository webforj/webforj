package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import java.util.Arrays;
import java.util.List;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.DwcFocusableComponent;
import org.dwcj.component.Expanse;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasFocusStatus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.HasTheme;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

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
    HasFocusStatus {
  private boolean disableOnClick = false;

  private final EventSinkListenerRegistry<ButtonClickEvent> clickEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ButtonClickEventSink(this, getEventDispatcher()),
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
        throw new DwcjRuntimeException(e);
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
        throw new DwcjRuntimeException(e);
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
  public T setName(String name) {
    setUnrestrictedAttribute("name", name);
    return getSelf();
  }

  /**
   * Gets the button name.
   *
   * @return the name of the button
   * @see #setName(String)
   */
  public String getName() {
    String name = getAttribute("name");
    return name == null ? "" : name;
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
      throw new DwcjRuntimeException(e);
    }
  }
}
