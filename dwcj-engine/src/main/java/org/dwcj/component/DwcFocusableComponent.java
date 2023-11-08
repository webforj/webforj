package org.dwcj.component;

import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.ListenerRegistration;
import org.dwcj.concern.HasEnablement;
import org.dwcj.concern.HasFocus;
import org.dwcj.concern.HasFocusStatus;

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
public abstract class DwcFocusableComponent<T extends DwcFocusableComponent<T>>
    extends DwcComponent<T> implements HasFocus<T>, HasEnablement<T> {
  private final DwcFocusableMixin<T> focusMixin = new DwcFocusableMixin<>(this);

  /**
   * {@inheritDoc}
   */
  @Override
  public T setEnabled(boolean enabled) {
    return focusMixin.setEnabled(enabled);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return focusMixin.isEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setFocusable(boolean focusable) {
    return focusMixin.setFocusable(focusable);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isFocusable() {
    return focusMixin.isFocusable();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T focus() {
    return focusMixin.focus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<FocusEvent> addFocusListener(
      ComponentEventListener<FocusEvent> listener) {
    return focusMixin.addFocusListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeFocusListener(ComponentEventListener<FocusEvent> listener) {
    return focusMixin.removeFocusListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<BlurEvent> addBlurListener(
      ComponentEventListener<BlurEvent> listener) {
    return focusMixin.addBlurListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeBlurListener(ComponentEventListener<BlurEvent> listener) {
    return focusMixin.removeBlurListener(listener);
  }

  /**
   * Checks if the component has focus.
   *
   * @see HasFocusStatus
   *
   * @return true if the component has focus, false if not.
   */
  protected boolean componentHasFocus() {
    return focusMixin.hasFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    focusMixin.attachControlCallbacks();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();
    focusMixin.onAttach();
  }
}
