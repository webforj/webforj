package com.webforj.component.field;

import java.util.Arrays;
import java.util.List;
import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcValidatableComponent;
import com.webforj.component.window.Window;
import com.webforj.concern.HasReadOnly;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * A {@link DwcField} that is used to initialize a {@link BBjEditBox} control.
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the field.
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
abstract class DwcFieldInitializer<T extends DwcValidatableComponent<T, V> & HasReadOnly<T>, V>
    extends DwcField<T, V> {

  /**
   * Constructs a new field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  DwcFieldInitializer(String label, V value, String placeholder) {
    super(label, value, placeholder);
  }

  /**
   * Constructs a new field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  DwcFieldInitializer(String label, V value, EventListener<ValueChangeEvent<V>> listener) {
    super(label, value, listener);
  }

  /**
   * Constructs a new field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  DwcFieldInitializer(String label, V value) {
    super(label, value);
  }

  /**
   * Constructs a new field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  DwcFieldInitializer(String label, EventListener<ValueChangeEvent<V>> listener) {
    super(label, listener);
  }

  /**
   * Constructs a new field with a value change listener.
   *
   * @param listener the value change listener
   */
  DwcFieldInitializer(EventListener<ValueChangeEvent<V>> listener) {
    super(listener);
  }

  /**
   * Constructs a new field with a label.
   *
   * @param label the label of the field
   */
  DwcFieldInitializer(String label) {
    super(label);
  }

  /**
   * Constructs a new field.
   */
  DwcFieldInitializer() {
    super();
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
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addEditBox(getText(), flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjEditBox", e);
    }
  }

  /**
   * Gets the instance of the underlying BBjEditBox control.
   *
   * @return the instance of the control
   */
  protected BBjEditBox inferField() {
    try {
      return (BBjEditBox) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
