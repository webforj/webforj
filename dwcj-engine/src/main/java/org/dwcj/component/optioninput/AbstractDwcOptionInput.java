package org.dwcj.component.optioninput;

import com.basis.bbj.proxies.sysgui.BBjToggleButton;
import com.basis.startup.type.BBjException;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.Expanse;
import org.dwcj.component.FocusableDwcComponent;
import org.dwcj.component.event.CheckEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.event.ListenerRegistration;
import org.dwcj.component.event.ToggleEvent;
import org.dwcj.component.event.UncheckEvent;
import org.dwcj.component.event.sink.CheckEventSink;
import org.dwcj.component.event.sink.ToggleEventSink;
import org.dwcj.component.event.sink.UncheckEventSink;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasTextPosition;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The Base class for all DWC (Dwcj Web Components) option input components.
 *
 * <p>
 * This abstract class serves as the foundation for all option input components within the
 * framework. It extends the {@link FocusableDwcComponent} class and implements various event
 * handling interfaces for working with option input-specific properties and behaviors.
 * </p>
 *
 * @param <T> The type of the component.
 *
 * @see FocusableDwcComponent
 * @see HasTextPosition
 * @see HasExpanse
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
abstract class AbstractDwcOptionInput<T extends FocusableDwcComponent<T> & HasTextPosition<T>>
    extends FocusableDwcComponent<T> implements HasTextPosition<T>, HasExpanse<T, Expanse> {

  private final EventSinkListenerRegistry<CheckEvent> checkEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new CheckEventSink(this, getEventDispatcher()),
          CheckEvent.class);
  private final EventSinkListenerRegistry<UncheckEvent> uncheckEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new UncheckEventSink(this, getEventDispatcher()),
          UncheckEvent.class);
  private final EventSinkListenerRegistry<ToggleEvent> toggleEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ToggleEventSink(this, getEventDispatcher()),
          ToggleEvent.class);

  private HasTextPosition.Position textPosition = HasTextPosition.Position.RIGHT;
  private boolean checked = false;

  /**
   * Create a new AbstractOptionInput component.
   *
   * @param text Desired text for the AbstractOptionInput.
   * @param checked True if the AbstractOptionInput should be created as checked, false otherwise.
   */
  protected AbstractDwcOptionInput(String text, boolean checked) {
    super();
    setText(text);
    setChecked(checked);
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * Checks or unchecks the component.
   *
   * @param checked true if checked, false if unchecked.
   *
   * @return The component itself
   */
  public T setChecked(boolean checked) {
    BBjToggleButton theControl = inferControl();

    if (theControl != null) {
      try {
        theControl.setSelected(checked);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.checked = checked;

    return getSelf();
  }

  /**
   * Checks if the component is checked.
   *
   * @return false if not checked, true if checked.
   */
  public boolean isChecked() {
    BBjToggleButton theControl = inferControl();

    if (theControl != null) {
      try {
        return theControl.isSelected();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this.checked;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setTextPosition(Position position) {
    BBjToggleButton theControl = inferControl();

    if (theControl != null) {
      try {
        theControl.setHorizontalTextPosition(0);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.textPosition = position;

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Position getTextPosition() {
    return this.textPosition;
  }

  /**
   * Sets the expanse for the component.
   *
   * @param expanse The component expanse
   * @return The component itself
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * Get the expanse of the component.
   *
   * @return The expanse for the component.
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return super.<Expanse>getComponentExpanse();
  }

  /**
   * Check if the component has focus.
   *
   * <p>
   * The method will always reach the client to get the focus state. If the component is not
   * attached to a panel, the method will return false even if the component {@link #focus()} method
   * was called.
   * </p>
   *
   * @return true if the component has focus, false if not.
   */
  public boolean hasFocus() {
    return Boolean.valueOf(String.valueOf(getProperty("hasFocus")));
  }

  /**
   * Add a {@link CheckEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<CheckEvent> addCheckListener(
      ComponentEventListener<CheckEvent> listener) {
    return this.checkEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addCheckListener(ComponentEventListener) addCheckedListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<CheckEvent> onCheck(ComponentEventListener<CheckEvent> listener) {
    return addCheckListener(listener);
  }

  /**
   * Remove a {@link CheckEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeCheckListener(ComponentEventListener<CheckEvent> listener) {
    this.checkEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add an {@link UncheckEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<UncheckEvent> addUncheckListener(
      ComponentEventListener<UncheckEvent> listener) {
    return this.uncheckEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addUncheckListener(ComponentEventListener) addUncheckedListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<UncheckEvent> onUncheck(
      ComponentEventListener<UncheckEvent> listener) {
    return addUncheckListener(listener);
  }

  /**
   * Remove an {@link UncheckEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeUncheckListener(ComponentEventListener<UncheckEvent> listener) {
    this.uncheckEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link ToggleEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ToggleEvent> addToggleListener(
      ComponentEventListener<ToggleEvent> listener) {
    return this.toggleEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addToggleListener(ComponentEventListener) addToggleListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ToggleEvent> onToggle(ComponentEventListener<ToggleEvent> listener) {
    return addToggleListener(listener);
  }

  /**
   * Remove a {@link ToggleEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeToggleListener(ComponentEventListener<ToggleEvent> listener) {
    this.toggleEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }


  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    this.checkEventSinkListenerRegistry.attach();
    this.uncheckEventSinkListenerRegistry.attach();
    this.toggleEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (this.checked) {
      this.setChecked(this.checked);
    }

    if (this.textPosition != Position.RIGHT) {
      this.setTextPosition(this.textPosition);
    }
  }

  protected BBjToggleButton inferControl() {
    try {
      return (BBjToggleButton) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}

