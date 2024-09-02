package com.webforj.component.field;

import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.component.Component;
import com.webforj.component.DwcValidatableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.event.ModifyEvent;
import com.webforj.component.event.sink.KeypressEventSink;
import com.webforj.component.event.sink.ModifyEventSink;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasHelperText;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasPlaceholder;
import com.webforj.concern.HasPrefixAndSuffix;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasRequired;
import com.webforj.data.concern.ValueChangeModeAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * The base class for all field components.
 *
 * <p>
 * This abstract class serves as the foundation for all field components within the framework. It
 * extends the {@link DwcValidatableComponent} class and implements several interfaces for handling
 * field-specific properties and behaviors.
 * </p>
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the field.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcField<T extends DwcValidatableComponent<T, V> & HasReadOnly<T>, V>
    extends DwcValidatableComponent<T, V> implements HasLabel<T>, HasReadOnly<T>, HasRequired<T>,
    HasExpanse<T, Expanse>, HasFocusStatus, HasHighlightOnFocus<T>, HasPlaceholder<T>,
    ValueChangeModeAware<T>, HasHelperText<T>, HasPrefixAndSuffix<T> {
  private final ComponentEventSinkRegistry<ModifyEvent> modifyEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ModifyEventSink(this, getEventDispatcher()),
          ModifyEvent.class);
  private final ComponentEventSinkRegistry<KeypressEvent> keypressEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new KeypressEventSink(this, getEventDispatcher()),
          KeypressEvent.class);

  private boolean autoFocus = false;
  private String label = "";
  private boolean required = false;
  private boolean spellcheck = false;
  private ValueChangeMode valueChangeMode = ValueChangeModeAware.ValueChangeMode.ON_MODIFY;
  private boolean registeredValueChangeModifiedListener = false;
  private boolean registeredValueChangeBlurListener = false;
  private String helperText = "";

  /**
   * Constructs a new field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  DwcField(String label, V value, String placeholder) {
    setLabel(label);
    setValue(value);
    setPlaceholder(placeholder);
    postInit();
  }

  /**
   * Constructs a new field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  DwcField(String label, V value, EventListener<ValueChangeEvent<V>> listener) {
    setLabel(label);
    setValue(value);
    if (listener != null) {
      addValueChangeListener(listener);
    }

    postInit();
  }

  /**
   * Constructs a new field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  DwcField(String label, V value) {
    setLabel(label);
    setValue(value);

    postInit();
  }

  /**
   * Constructs a new field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  DwcField(String label, EventListener<ValueChangeEvent<V>> listener) {
    setLabel(label);
    if (listener != null) {
      addValueChangeListener(listener);
    }

    postInit();
  }

  /**
   * Constructs a new field with a value change listener.
   *
   * @param listener the value change listener
   */
  DwcField(EventListener<ValueChangeEvent<V>> listener) {
    if (listener != null) {
      addValueChangeListener(listener);
    }

    postInit();
  }

  /**
   * Constructs a new field with a label.
   *
   * @param label the label of the field
   */
  DwcField(String label) {
    setLabel(label);
    postInit();
  }

  /**
   * Constructs a new field.
   */
  DwcField() {
    super();
    postInit();
  }

  private void postInit() {
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
   * Set the placeholder of field.
   *
   * @param placeholder the placeholder of field
   * @return the field type
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setPlaceholder(String placeholder) {
    setComponentPlaceholder(placeholder);
    return getSelf();
  }

  /**
   * Get the placeholder of field.
   *
   * @return the placeholder of field
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getPlaceholder() {
    return getComponentPlaceholder();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Behavior getHighlightOnFocus() {
    return getComponentHighlightOnFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
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
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
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
  public Component getPrefixComponent() {
    return super.getPrefixComponent();
  }

  /**
   * {@inheritDoc}
   *
   * @since 24.11
   */
  @Override
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
  public Component getSuffixComponent() {
    return super.getSuffixComponent();
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
