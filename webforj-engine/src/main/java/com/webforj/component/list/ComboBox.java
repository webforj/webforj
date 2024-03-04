package com.webforj.component.list;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.event.ModifyEvent;
import com.webforj.component.event.sink.KeypressEventSink;
import com.webforj.component.event.sink.ModifyEventSink;
import com.webforj.component.list.event.ListSelectEvent;
import com.webforj.component.window.Window;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasPlaceholder;
import com.webforj.concern.HasReadOnly;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.DwcjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a ComboBox component that allows user selection from a drop-down list of options or
 * the option to freely enter a new value not included in the predefined list.
 *
 * <p>
 * A ComboBox is a UI component that combines a text field with a dropdown list of options, giving
 * users the choice to either select from the list or input their own value. This versatility makes
 * ComboBoxes suitable for a wide range of scenarios where users need to pick from predefined
 * options or provide custom input. In contrast, a {@link ChoiceBox} presents users with a fixed
 * list of choices for selection, and it doesn't allow the input of values outside this predefined
 * list, making it more suitable for situations where custom input is not required.
 * </p>
 *
 * <p>
 * ComboBoxes are commonly employed in various applications, such as data entry forms, search bars,
 * and selection menus, providing an intuitive and flexible way for users to make selections or
 * input information.
 * </p>
 *
 * @see DwcSelectDropdown
 * @see ChoiceBox
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class ComboBox extends DwcSelectDropdown<ComboBox>
    implements HasReadOnly<ComboBox>, HasHighlightOnFocus<ComboBox>, HasPlaceholder<ComboBox> {

  private final EventSinkListenerRegistry<ModifyEvent> modifyEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ModifyEventSink(this, getEventDispatcher()),
          ModifyEvent.class);
  private final EventSinkListenerRegistry<KeypressEvent> keypressEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new KeypressEventSink(this, getEventDispatcher()),
          KeypressEvent.class);

  private boolean allowCustomValue = true;

  /**
   * Constructs a new ComboBox.
   */
  public ComboBox() {
    super();
    configureLisType();
  }

  /**
   * Constructs a new ComboBox with the given label.
   *
   * @param label the label
   */
  public ComboBox(String label) {
    super(label);
    configureLisType();
  }

  /**
   * Constructs a new ComboBox with the given label and select listener.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  public ComboBox(String label, EventListener<ListSelectEvent> selectListener) {
    super(label, selectListener);
    configureLisType();
  }

  /**
   * When false, the user won't be able to change the input's text.
   *
   * @param allowCustomValue true to allow custom value, false otherwise
   * @return the component itself
   */
  public ComboBox setAllowCustomValue(boolean allowCustomValue) {
    this.allowCustomValue = allowCustomValue;
    setUnrestrictedProperty("customValue", allowCustomValue);
    return this;
  }

  /**
   * Checks whether the user is allowed to enter custom value.
   *
   * @return true if the user is allowed to enter custom value, false otherwise
   */
  public boolean isAllowCustomValue() {
    return allowCustomValue;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ComboBox setPlaceholder(String placeholder) {
    return super.setComponentPlaceholder(placeholder);
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public String getPlaceholder() {
    return super.getComponentPlaceholder();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ComboBox setReadOnly(boolean readonly) {
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
  @ExcludeFromJacocoGeneratedReport
  public ComboBox setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
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
   * Adds a {@link ModifyEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ModifyEvent> addModifyListener(EventListener<ModifyEvent> listener) {
    return modifyEventSinkListenerRegistry.addEventListener(listener);
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
   * Adds a {@link KeypressEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<KeypressEvent> addKeypressListener(
      EventListener<KeypressEvent> listener) {
    return keypressEventSinkListenerRegistry.addEventListener(listener);
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
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "customValue", "disabled", "distance", "expanse", "fieldHeight", "hasFocus",
        "highlightBehaviors", "invalid", "invalidMessage", "itemLabel", "itemValue", "items",
        "label", "maxRowCount", "maxlength", "openHeight", "openWidth", "opened", "placeholder",
        "placement", "readonly", "renderer", "selected", "skidding", "toggleOnEnter", "type",
        "valid", "validationIcon", "validationPopoverDistance", "validationPopoverPlacement",
        "validationPopoverSkidding", "validationStyle", "validator", "value"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    modifyEventSinkListenerRegistry.attach();
    keypressEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addListEdit("", flags));
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create the BBjListEdit Control", e);
    }
  }

  private void configureLisType() {
    setDropdownType("ComboBox");
    setUnrestrictedProperty("fieldHeight", "");
  }
}
