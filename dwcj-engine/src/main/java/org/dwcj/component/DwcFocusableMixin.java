package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.Focusable;
import com.basis.startup.type.BBjException;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.concern.HasEnablement;
import org.dwcj.concern.HasFocus;
import org.dwcj.concern.HasFocusStatus;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

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
  private final EventSinkListenerRegistry<FocusEvent> focusEventSinkListenerRegistry;
  private final EventSinkListenerRegistry<BlurEvent> blurEventSinkListenerRegistry;
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
    focusEventSinkListenerRegistry = new EventSinkListenerRegistry<>(
        new FocusEventSink(component, component.getEventDispatcher()), FocusEvent.class);
    blurEventSinkListenerRegistry = new EventSinkListenerRegistry<>(
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
        throw new DwcjRuntimeException(e);
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
    BBjControl control = component.getControl();

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
        throw new DwcjRuntimeException(e);
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
