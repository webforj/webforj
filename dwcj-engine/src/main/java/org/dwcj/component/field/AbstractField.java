package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.util.Arrays;
import java.util.List;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Expanse;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasExpanse;
import org.dwcj.component.HasFocus;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.HasValue;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.EventSinkListenerRegistry;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.KeypressEventSink;
import org.dwcj.component.event.sink.ModifyEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * The Base class for all fields components.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
abstract class AbstractField<T extends AbstractDwcComponent & HasFocus & TabTraversable & HasEnable & HasReadOnly, V>
    extends AbstractDwcComponent implements DwcjFieldComponent, HasEnable, HasReadOnly, HasFocus,
    TabTraversable, HasExpanse<T, Expanse>, HasValue<T, V> {

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkListenerRegistry<ModifyEvent> modifyEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ModifyEventSink(this, dispatcher), ModifyEvent.class);
  private EventSinkListenerRegistry<KeypressEvent> keypressEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new KeypressEventSink(this, dispatcher), KeypressEvent.class);
  private EventSinkListenerRegistry<FocusEvent> focusEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new FocusEventSink(this, dispatcher), FocusEvent.class);
  private EventSinkListenerRegistry<BlurEvent> blurEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new BlurEventSink(this, dispatcher), BlurEvent.class);
  private EventSinkListenerRegistry<MouseEnterEvent> mouseEnterEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseEnterEventSink(this, dispatcher),
          MouseEnterEvent.class);
  private EventSinkListenerRegistry<MouseExitEvent> mouseExitEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseExitEventSink(this, dispatcher),
          MouseExitEvent.class);
  private EventSinkListenerRegistry<RightMouseDownEvent> rightMouseDownEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);


  private boolean autoFocus = false;
  private String label = "";
  private boolean required = false;
  private boolean spellcheck = false;

  /**
   * Construct a new field.
   */
  protected AbstractField() {
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * Set the field's label.
   *
   * <p>
   * A field label is a descriptive text or title that is associated with the field. It provides a
   * brief explanation or prompt to help users understand the purpose or expected input for that
   * particular field. Field labels are not only important for usability but also play a crucial
   * role in accessibility, as they enable screen readers and assistive technologies to provide
   * accurate information and facilitate keyboard navigation.
   * </p>
   *
   * @param label the label
   * @return the component itself
   */
  public T setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", this.label);

    return getSelf();
  }

  /**
   * Get the field's label.
   *
   * @return the field's label
   */
  public String getLabel() {
    return this.label;
  }

  /**
   * Set the field's required state.
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
   * Check if the field is required.
   *
   * @return true if the field is required, false otherwise
   */
  public boolean isRequired() {
    return this.required;
  }

  /**
   * Set whether the field's value may be checked for spelling errors.
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
   * Check if the field has spellcheck enabled.
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
    return (Expanse) getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T focus() {
    super.focusComponent();
    return getSelf();
  }

  /**
   * When true, should automatically have focus when the app has finished loading.
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
   * Check if the component should automatically have focus when the app has finished loading.
   *
   * @return true if the component should automatically have focus when the app has finished
   *         loading.
   */
  public boolean isAutoFocus() {
    return this.autoFocus;
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
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTabTraversable(Boolean traversable) {
    super.setComponentTabTraversable(traversable);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isTabTraversable() {
    return super.isComponentTabTraversable();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setReadOnly(Boolean readonly) {
    super.setComponentReadOnly(readonly);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isReadOnly() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setText(String text) {
    super.setText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setVisible(Boolean visible) {
    super.setVisible(visible);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTooltipText(String text) {
    super.setTooltipText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setProperty(String property, Object value) {
    super.setProperty(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setStyle(String property, String value) {
    super.setStyle(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T addClassName(String selector) {
    super.addClassName(selector);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T removeClassName(String selector) {
    super.removeClassName(selector);
    return getSelf();
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
   * Get the event dispatcher instance for the component.
   *
   * @return The instance of the event dispatcher.
   */
  EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Add a {@link ModifyEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addModifyListener(EventListener<ModifyEvent> listener) {
    this.modifyEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addModifyListener(EventListener) addModifyListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onModify(EventListener<ModifyEvent> listener) {
    return addModifyListener(listener);
  }

  /**
   * Removes a {@link ModifyEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeModifyListener(EventListener<ModifyEvent> listener) {
    this.modifyEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link KeypressEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addKeypressListener(EventListener<KeypressEvent> listener) {
    this.keypressEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addKeypressListener(EventListener) addKeypressListener}.
   *
   * @see AbstractField #addKeypressListener(EventListener)
   * @param listener The event listener to be removed
   *
   * @return the component itself
   */
  public T onKeypress(EventListener<KeypressEvent> listener) {
    return addKeypressListener(listener);
  }

  /**
   * Removes a {@link KeypressEvent} listener from the component.
   *
   * @param listener The event listener to be removed
   * @return The component itself
   */
  public T removeKeypressListener(EventListener<KeypressEvent> listener) {
    this.keypressEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link FocusEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addFocusListener(EventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a {@link FocusEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link BlurEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addBlurListener(EventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a {@link BlurEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Adds a {@link MouseEnterEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addMouseEnterListener(EventListener) addMouseEnterListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Remove a {@link MouseEnterEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link MouseExitEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addMouseExitListener(EventListener) addMouseExitListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Remove a {@link MouseExitEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link RightMouseDownEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addRightMouseDownListener(EventListener) addRightMouseDownListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Remove a {@link RightMouseDownEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addEditBox(getText(), flags));
      catchUp();
    } catch (BBjException | IllegalAccessException e) {
      throw new DwcjRuntimeException("Failed to create BBjEditBox", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }

    // catch up event listeners
    this.modifyEventSinkListenerRegistry.catchUp();
    this.keypressEventSinkListenerRegistry.catchUp();
    this.focusEventSinkListenerRegistry.catchUp();
    this.blurEventSinkListenerRegistry.catchUp();
    this.mouseEnterEventSinkListenerRegistry.catchUp();
    this.mouseExitEventSinkListenerRegistry.catchUp();
    this.rightMouseDownEventSinkListenerRegistry.catchUp();

    super.catchUp();
  }

  protected BBjEditBox getBbjControl() {
    try {
      return (BBjEditBox) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  protected T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}
