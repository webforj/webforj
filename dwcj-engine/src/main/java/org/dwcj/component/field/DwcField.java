package org.dwcj.component.field;

import java.util.Arrays;
import java.util.List;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.Expanse;
import org.dwcj.component.DwcFocusableComponent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ListenerRegistration;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.component.event.sink.KeypressEventSink;
import org.dwcj.component.event.sink.ModifyEventSink;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasFocusStatus;
import org.dwcj.concern.HasLabel;
import org.dwcj.concern.HasReadOnly;
import org.dwcj.concern.HasValue;

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
public abstract class DwcField<T extends DwcFocusableComponent<T> & HasReadOnly<T>, V>
    extends DwcFocusableComponent<T>
    implements HasLabel<T>, HasValue<T, V>, HasReadOnly<T>, HasExpanse<T, Expanse>, HasFocusStatus {

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
  public ListenerRegistration<ModifyEvent> addModifyListener(
      ComponentEventListener<ModifyEvent> listener) {
    return this.modifyEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addModifyListener(ComponentEventListener) addModifyListener}.
   *
   * @param listener the event listener to be added
   * @return @return A registration object for removing the event listener
   */
  public ListenerRegistration<ModifyEvent> onModify(ComponentEventListener<ModifyEvent> listener) {
    return addModifyListener(listener);
  }

  /**
   * Removes a {@link ModifyEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeModifyListener(ComponentEventListener<ModifyEvent> listener) {
    this.modifyEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Adds a {@link KeypressEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<KeypressEvent> addKeypressListener(
      ComponentEventListener<KeypressEvent> listener) {
    return this.keypressEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addKeypressListener(ComponentEventListener) addKeypressListener}.
   *
   * @param listener The event listener to be removed
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<KeypressEvent> onKeypress(
      ComponentEventListener<KeypressEvent> listener) {
    return addKeypressListener(listener);
  }

  /**
   * Removes a {@link KeypressEvent} listener from the component.
   *
   * @param listener The event listener to be removed
   * @return The component itself
   */
  public T removeKeypressListener(ComponentEventListener<KeypressEvent> listener) {
    this.keypressEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
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
}
