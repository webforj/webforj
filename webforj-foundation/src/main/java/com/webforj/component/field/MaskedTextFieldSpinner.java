package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a masked text field with an associated spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 *
 * @see MaskedTextField
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedTextFieldSpinner extends MaskedTextField
    implements HasOptions<MaskedTextFieldSpinner>, Spinnable<MaskedTextFieldSpinner> {
  private HasOptionsMixin hasOptionsMixin;
  private SpinnableMixin spinnableMixin;

  /**
   * Constructs a new masked text field spinner with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedTextFieldSpinner(String label, String value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedTextFieldSpinner(String label, String value,
      EventListener<ValueChangeEvent<String>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedTextFieldSpinner(String label, String value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedTextFieldSpinner(String label, EventListener<ValueChangeEvent<String>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedTextFieldSpinner(EventListener<ValueChangeEvent<String>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner with a label.
   *
   * @param label the label of the field
   */
  public MaskedTextFieldSpinner(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked text field spinner.
   */
  public MaskedTextFieldSpinner() {
    super();
    postInit();
  }

  MaskedTextFieldSpinner(HasOptionsMixin hasOptionsMixin, SpinnableMixin spinnableMixin) {
    super();
    this.hasOptionsMixin = hasOptionsMixin;
    this.spinnableMixin = spinnableMixin;
  }

  private void postInit() { // NOSONAR
    this.hasOptionsMixin = new HasOptionsMixin(this);
    this.spinnableMixin = new SpinnableMixin(this);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextFieldSpinner setOptions(List<String> list) {
    hasOptionsMixin.setOptions(list);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getOptions() {
    return hasOptionsMixin.getOptions();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextFieldSpinner setOptionIndex(int index) {
    hasOptionsMixin.setOptionIndex(index);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOptionIndex() {
    return hasOptionsMixin.getOptionIndex();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextFieldSpinner spinUp() {
    spinnableMixin.spinUp();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextFieldSpinner spinDown() {
    spinnableMixin.spinDown();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("list", "listIndex", "wrap"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();
    hasOptionsMixin.onAttach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputESpinner(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputESpinner", e);
    }
  }
}
