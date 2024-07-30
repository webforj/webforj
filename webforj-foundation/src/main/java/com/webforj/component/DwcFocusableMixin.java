package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.Focusable;
import com.basis.startup.type.BBjException;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.event.FocusEvent;
import com.webforj.component.event.sink.BlurEventSink;
import com.webforj.component.event.sink.FocusEventSink;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasFocus;
import com.webforj.concern.HasFocusStatus;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * This class encapsulates the focusable behavior of DWC components. It manages the component's
 * focus and blur events, as well as its ability to be enabled or disabled. It also supports
 * registering and removing event listeners for focus and blur events.
 *
 * <p>
 * Instances of this class are associated with a specific {@link DwcComponent}, and they interact
 * with the component's underlying BBjControl to implement the {@link HasFocus},
 * {@link HasFocusStatus}, and {@link HasEnablement} interfaces.
 * </p>
 *
 * @param <T> the type of the DWC component which extends {@link DwcComponent}
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class DwcFocusableMixin<T extends DwcComponent<T>>
    implements HasFocus<T>, HasFocusStatus, HasEnablement<T> {
  private final DwcComponent<T> component;
  private final ComponentEventSinkRegistry<FocusEvent> focusEventSinkListenerRegistry;
  private final ComponentEventSinkRegistry<BlurEvent> blurEventSinkListenerRegistry;
  private boolean enabled = true;
  private boolean focusable = true;
  private Boolean wasFocused = null;

  /**
   * Constructor a new {@link DwcFocusableMixin}.
   *
   * @param component the component to which this behavior is attached
   */
  public DwcFocusableMixin(DwcComponent<T> component) {
    this.component = component;
    focusEventSinkListenerRegistry = new ComponentEventSinkRegistry<>(
        new FocusEventSink(component, component.getEventDispatcher()), FocusEvent.class);
    blurEventSinkListenerRegistry = new ComponentEventSinkRegistry<>(
        new BlurEventSink(component, component.getEventDispatcher()), BlurEvent.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setEnabled(boolean enabled) {
    if (component.getControl() != null) {
      try {
        component.getControl().setEnabled(enabled);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.enabled = enabled;
    return component.getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    if (component.getControl() != null) {
      try {
        return component.getControl().isEnabled();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return enabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setFocusable(boolean focusable) {
    BBjControl control = component.getControl();

    if (control != null) {
      try {
        if (control instanceof Focusable) {
          ((Focusable) control).setFocusable(focusable);
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.focusable = focusable;
    return component.getSelf();
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
    BBjControl control = component.getControl();

    if (control != null) {
      try {
        control.focus();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.wasFocused = true;
    return component.getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<FocusEvent> addFocusListener(EventListener<FocusEvent> listener) {
    return this.focusEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<BlurEvent> addBlurListener(EventListener<BlurEvent> listener) {
    return this.blurEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasFocus() {
    return Boolean.valueOf(String.valueOf(component.getProperty("hasFocus")));
  }

  /**
   * Attaches the component callbacks to the underlying BBj control.
   */
  public void attachControlCallbacks() {
    focusEventSinkListenerRegistry.attach();
    blurEventSinkListenerRegistry.attach();
  }

  /**
   * Catchup with component changes after the component is attached.
   */
  public void onAttach() {
    if (!Boolean.TRUE.equals(enabled)) {
      setEnabled(enabled);
    }

    if (this.wasFocused != null) {
      this.focus();
    }
  }
}
