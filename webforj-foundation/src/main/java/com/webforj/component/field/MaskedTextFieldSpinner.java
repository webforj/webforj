package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
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
  private final HasOptionsMixin hasOptionsMixin;
  private final SpinnableMixin spinnableMixin;

  MaskedTextFieldSpinner(HasOptionsMixin hasOptionsMixin, SpinnableMixin spinnableMixin) {
    super();
    this.hasOptionsMixin = hasOptionsMixin;
    this.spinnableMixin = spinnableMixin;
  }

  /**
   * Constructs a new masked text field spinner.
   */
  public MaskedTextFieldSpinner() {
    super();
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
