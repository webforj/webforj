package com.webforj.component.optioninput;

import com.basis.bbj.proxies.sysgui.BBjToggleButton;
import com.basis.startup.type.BBjException;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.DwcValidatableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.event.CheckEvent;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.event.ToggleEvent;
import com.webforj.component.event.UncheckEvent;
import com.webforj.component.event.sink.CheckEventSink;
import com.webforj.component.event.sink.ToggleEventSink;
import com.webforj.component.event.sink.UncheckEventSink;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasHelperText;
import com.webforj.concern.HasTextPosition;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * The Base class for all DWC option input components.
 *
 * <p>
 * This abstract class serves as the foundation for all option input components within the
 * framework. It extends the {@link DwcFocusableComponent} class and implements various event
 * handling interfaces for working with option input-specific properties and behaviors.
 * </p>
 *
 * @param <T> The type of the component.
 *
 * @see DwcFocusableComponent
 * @see HasTextPosition
 * @see HasExpanse
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcOptionInput<T extends DwcValidatableComponent<T, Boolean> & HasTextPosition<T>>
    extends DwcValidatableComponent<T, Boolean>
    implements HasTextPosition<T>, HasExpanse<T, Expanse>, HasFocusStatus, HasHelperText<T> {

  private final ComponentEventSinkRegistry<CheckEvent> checkEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new CheckEventSink(this, getEventDispatcher()),
          CheckEvent.class);
  private final ComponentEventSinkRegistry<UncheckEvent> uncheckEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new UncheckEventSink(this, getEventDispatcher()),
          UncheckEvent.class);
  private final ComponentEventSinkRegistry<ToggleEvent> toggleEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ToggleEventSink(this, getEventDispatcher()),
          ToggleEvent.class);

  private HasTextPosition.Position textPosition = HasTextPosition.Position.RIGHT;
  private boolean checked = false;
  private boolean registeredToggleValueChangeListener = false;
  private String helperText = "";

  /**
   * Creates a new DwcOptionInput component.
   *
   * @param text Desired text for the DwcOptionInput.
   * @param checked True if the DwcOptionInput should be created as checked, false otherwise.
   */
  DwcOptionInput(String text, boolean checked, EventListener<ValueChangeEvent<Boolean>> listener) {
    super();
    setText(text);
    setChecked(checked);
    setExpanse(Expanse.MEDIUM);

    if (listener != null) {
      addValueChangeListener(listener);
    }
  }

  /**
   * Creates a new DwcOptionInput component.
   *
   * @param text Desired text for the DwcOptionInput.
   * @param checked True if the DwcOptionInput should be created as checked, false otherwise.
   */
  DwcOptionInput(String text, boolean checked) {
    this(text, checked, null);
  }

  /**
   * Creates a new DwcOptionInput component.
   *
   * @param text The text for the DwcOptionInput.
   */
  DwcOptionInput(String text) {
    this(text, false);
  }

  /**
   * Creates a new DwcOptionInput component.
   *
   * @param checked True if the DwcOptionInput should be created as checked, false otherwise.
   */
  DwcOptionInput(boolean checked) {
    this("", checked);
  }

  /**
   * Creates a new DwcOptionInput component.
   */
  DwcOptionInput() {
    this("");
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
        throw new WebforjRuntimeException(e);
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
        throw new WebforjRuntimeException(e);
      }
    }

    return this.checked;
  }

  /**
   * Alias for {@link #setChecked(boolean) setChecked}.
   *
   * @param value true if checked, false if unchecked.
   */
  @Override
  public T setValue(Boolean value) {
    return setChecked(value);
  }

  /**
   * Alias for {@link #isChecked() isChecked}.
   *
   * @return false if not checked, true if checked.
   */
  @Override
  public Boolean getValue() {
    return isChecked();
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
        throw new WebforjRuntimeException(e);
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
   * Gets the expanse of the component.
   *
   * @return The expanse for the component.
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
  public boolean hasFocus() {
    return componentHasFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setHelperText(String helperText) {
    this.helperText = helperText;
    setUnrestrictedProperty("helperText", this.helperText);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHelperText() {
    return this.helperText;
  }

  /**
   * Adds a {@link CheckEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<CheckEvent> addCheckListener(EventListener<CheckEvent> listener) {
    return this.checkEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addCheckListener(EventListener) addCheckedListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<CheckEvent> onCheck(EventListener<CheckEvent> listener) {
    return addCheckListener(listener);
  }

  /**
   * Adds an {@link UncheckEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<UncheckEvent> addUncheckListener(
      EventListener<UncheckEvent> listener) {
    return this.uncheckEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addUncheckListener(EventListener) addUncheckedListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<UncheckEvent> onUncheck(EventListener<UncheckEvent> listener) {
    return addUncheckListener(listener);
  }

  /**
   * Adds a {@link ToggleEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ToggleEvent> addToggleListener(EventListener<ToggleEvent> listener) {
    return this.toggleEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addToggleListener(EventListener) addToggleListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ToggleEvent> onToggle(EventListener<ToggleEvent> listener) {
    return addToggleListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<ValueChangeEvent<Boolean>> addValueChangeListener(
      EventListener<ValueChangeEvent<Boolean>> listener) {
    ListenerRegistration<ValueChangeEvent<Boolean>> registration =
        getEventDispatcher().addListener(ValueChangeEvent.class, listener);

    if (!registeredToggleValueChangeListener) {
      addToggleListener(ev -> {
        ValueChangeEvent<Boolean> valueChangeEvent = new ValueChangeEvent<>(this, ev.isToggled());
        getEventDispatcher().dispatchEvent(valueChangeEvent, (l, e) -> l.equals(listener));
      });

      registeredToggleValueChangeListener = true;
    }

    return registration;
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

  private BBjToggleButton inferControl() {
    try {
      return (BBjToggleButton) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}

