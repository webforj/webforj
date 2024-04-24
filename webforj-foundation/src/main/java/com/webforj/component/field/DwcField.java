package com.webforj.component.field;

import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.DwcValidatableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.event.ModifyEvent;
import com.webforj.component.event.sink.KeypressEventSink;
import com.webforj.component.event.sink.ModifyEventSink;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasRequired;
import com.webforj.concern.HasValue;
import com.webforj.data.concern.ValueChangeModeAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Arrays;
import java.util.List;

/**
 * The base class for all field components.
 *
 * <p>
 * This abstract class serves as the foundation for all field components within the framework. It
 * extends the {@link DwcFocusableComponent} class and implements several interfaces for handling
 * field-specific properties and behaviors.
 * </p>
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the field.
 *
 * @see DwcFocusableComponent
 * @see FieldComponent
 * @see HasValue
 * @see HasExpanse
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcField<T extends DwcValidatableComponent<T, V> & HasReadOnly<T>, V>
    extends DwcValidatableComponent<T, V> implements HasLabel<T>, HasReadOnly<T>, HasRequired<T>,
    HasExpanse<T, Expanse>, HasFocusStatus, ValueChangeModeAware<T> {

  private final EventSinkListenerRegistry<ModifyEvent> modifyEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ModifyEventSink(this, getEventDispatcher()),
          ModifyEvent.class);
  private final EventSinkListenerRegistry<KeypressEvent> keypressEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new KeypressEventSink(this, getEventDispatcher()),
          KeypressEvent.class);

  private boolean autoFocus = false;
  private String label = "";
  private boolean required = false;
  private boolean spellcheck = false;
  private ValueChangeMode valueChangeMode = ValueChangeModeAware.ValueChangeMode.ON_MODIFY;
  private boolean registeredValueChangeModifiedListener = false;
  private boolean registeredValueChangeBlurListener = false;

  /**
   * Constructs a new field with a default medium expanse.
   */
  protected DwcField() {
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", this.label);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel() {
    return this.label;
  }

  /**
   * Sets the field's required state.
   *
   * <p>
   * A field is required when the user must provide a value before submitting a form. This is mainly
   * used in conjunction with the {@link #setLabel(String)} to provide a visual indication to users
   * that the field is required.
   * </p>
   *
   * @param required true if the field is required, false otherwise
   * @return the component itself
   */
  @Override
  public T setRequired(boolean required) {
    this.required = required;
    setUnrestrictedProperty("required", this.required);

    return getSelf();
  }

  /**
   * Checks if the field is required.
   *
   * @return true if the field is required, false otherwise
   */
  @Override
  public boolean isRequired() {
    return this.required;
  }

  /**
   * Sets whether the field's value may be checked for spelling errors.
   *
   * @param spellcheck true if the field's value may be checked for spelling errors, false
   *        otherwise.
   * @return the component itself
   */
  public T setSpellCheck(boolean spellcheck) {
    this.spellcheck = spellcheck;
    setUnrestrictedProperty("spellcheck", this.spellcheck);

    return getSelf();
  }

  /**
   * Checks if the field has spellcheck enabled.
   *
   * @return true if the field is spellcheck, false otherwise
   */
  public boolean isSpellCheck() {
    return this.spellcheck;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Expanse getExpanse() {
    return super.<Expanse>getComponentExpanse();
  }

  /**
   * When true, the component should automatically have focus when the app has finished loading.
   *
   * @param autofocus true to automatically have focus when the app has finished loading.
   * @return the component itself
   */
  public T setAutoFocus(boolean autofocus) {
    this.autoFocus = autofocus;
    setUnrestrictedProperty("autofocus", autofocus);
    return getSelf();
  }

  /**
   * Checks if the component should automatically have focus when the app has finished loading.
   *
   * @return true if the component should automatically have focus when the app has finished
   *         loading.
   */
  public boolean isAutoFocus() {
    return this.autoFocus;
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
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setReadOnly(boolean readonly) {
    return super.setComponentReadOnly(readonly);
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public boolean isReadOnly() {
    return super.isComponentReadOnly();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setValueChangeMode(ValueChangeMode valueChangeMode) {
    this.valueChangeMode = valueChangeMode;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ValueChangeMode getValueChangeMode() {
    return this.valueChangeMode;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("accept", "autoValidate", "autoValidateOnLoad",
        "autoWasValidated", "autocomplete", "autocorrect", "autofocus", "disabled", "expanse",
        "hasFocus", "highlightBehaviors", "invalid", "invalidMessage", "label", "max", "maxlength",
        "min", "minlength", "multiple", "name", "passwordReveal", "pattern", "placeholder",
        "readonly", "required", "showSpinners", "size", "spellcheck", "spinnable", "step",
        "tabTraversable", "type", "valid", "validationIcon", "validationPopoverDistance",
        "validationPopoverPlacement", "validationPopoverSkidding", "validationStyle", "validator",
        "value"));

    return properties;
  }

  /**
   * Adds a {@link ModifyEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ModifyEvent> addModifyListener(EventListener<ModifyEvent> listener) {
    return this.modifyEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addModifyListener(EventListener) addModifyListener}.
   *
   * @param listener the event listener to be added
   * @return @return A registration object for removing the event listener
   */
  public ListenerRegistration<ModifyEvent> onModify(EventListener<ModifyEvent> listener) {
    return addModifyListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<ValueChangeEvent<V>> addValueChangeListener(
      EventListener<ValueChangeEvent<V>> listener) {
    ListenerRegistration<ValueChangeEvent<V>> registration =
        getEventDispatcher().addListener(ValueChangeEvent.class, listener);

    ValueChangeMode mode = getValueChangeMode();
    switch (mode) {
      case ON_MODIFY:
        if (!this.registeredValueChangeModifiedListener) {
          addModifyListener(ev -> fireValueChangeEvent(ev.getText()));
          this.registeredValueChangeModifiedListener = true;
        }

        break;
      case ON_BLUR:
        if (!this.registeredValueChangeBlurListener) {
          addBlurListener(ev -> fireValueChangeEvent(ev.getText()));
          this.registeredValueChangeBlurListener = true;
        }

        break;
      default:
        break;
    }

    return registration;
  }

  /**
   * Adds a {@link KeypressEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<KeypressEvent> addKeypressListener(
      EventListener<KeypressEvent> listener) {
    return this.keypressEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addKeypressListener(EventListener) addKeypressListener}.
   *
   * @param listener The event listener to be removed
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<KeypressEvent> onKeypress(EventListener<KeypressEvent> listener) {
    return addKeypressListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    this.modifyEventSinkListenerRegistry.attach();
    this.keypressEventSinkListenerRegistry.attach();
  }

  /**
   * Converts the value from a string to the appropriate type.
   *
   * @param value the value to be converted
   * @return the converted value
   */
  protected abstract V convertValue(String value);

  private void fireValueChangeEvent(String text) {
    V value = null;
    try {
      value = convertValue(text);
    } catch (Exception e) {
      // ignore
    }

    ValueChangeEvent<V> valueChangeEvent = new ValueChangeEvent<>(this, value);
    getEventDispatcher().dispatchEvent(valueChangeEvent);
  }
}
