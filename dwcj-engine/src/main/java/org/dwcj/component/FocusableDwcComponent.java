package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.Focusable;
import com.basis.startup.type.BBjException;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.ListenerRegistration;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.concern.HasEnablement;
import org.dwcj.concern.HasFocus;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * An abstract class for typed DWC components that can receive focus and trigger focus-related
 * events.
 *
 * <p>
 * <strong>Important:</strong> Same inheritance rules of {@link DwcComponent} apply to this class.
 * </p>
 *
 * @param <T> The type of the component itself, allowing for fluent method chaining.
 *
 * @see DwcComponent
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class FocusableDwcComponent<T extends FocusableDwcComponent<T>>
    extends DwcComponent<T> implements HasEnablement<T>, HasFocus<T> {

  private final EventSinkListenerRegistry<FocusEvent> focusEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new FocusEventSink(this, getEventDispatcher()),
          FocusEvent.class);
  private final EventSinkListenerRegistry<BlurEvent> blurEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new BlurEventSink(this, getEventDispatcher()),
          BlurEvent.class);

  private boolean enabled = true;
  private boolean focusable = true;
  private Boolean wasFocused = null;

  /**
   * {@inheritDoc}
   */
  @Override
  public T setEnabled(boolean enabled) {
    if (getControl() != null) {
      try {
        getControl().setEnabled(enabled);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.enabled = enabled;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    if (getControl() != null) {
      try {
        return getControl().isEnabled();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return enabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setFocusable(boolean focusable) {
    BBjControl control = getControl();

    if (control != null) {
      try {
        if (control instanceof Focusable) {
          ((Focusable) control).setFocusable(focusable);
        }
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.focusable = focusable;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isFocusable() {
    return focusable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T focus() {
    BBjControl control = getControl();

    if (control != null) {
      try {
        control.focus();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.wasFocused = true;
    return getSelf();
  }

  /**
   * Adds a {@link FocusEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<FocusEvent> addFocusListener(
      ComponentEventListener<FocusEvent> listener) {
    return this.focusEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addFocusListener(ComponentEventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<FocusEvent> onFocus(ComponentEventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a {@link FocusEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeFocusListener(ComponentEventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Adds a {@link BlurEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<BlurEvent> addBlurListener(
      ComponentEventListener<BlurEvent> listener) {
    return this.blurEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addBlurListener(ComponentEventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<BlurEvent> onBlur(ComponentEventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a {@link BlurEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeBlurListener(ComponentEventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Checks if the component has focus.
   *
   * @see HasFocusStatus
   *
   * @return true if the component has focus, false if not.
   */
  protected boolean componentHasFocus() {
    return Boolean.valueOf(String.valueOf(getProperty("hasFocus")));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();

    focusEventSinkListenerRegistry.attach();
    blurEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (!Boolean.TRUE.equals(enabled)) {
      setEnabled(enabled);
    }

    if (this.wasFocused != null) {
      this.focus();
    }
  }
}
