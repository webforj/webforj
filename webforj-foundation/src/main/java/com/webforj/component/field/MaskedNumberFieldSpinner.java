package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasStep;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Objects;

/**
 * Represents a masked number field with an associated spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 *
 * @see MaskedNumberField
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedNumberFieldSpinner extends MaskedNumberField
    implements HasStep<MaskedNumberFieldSpinner, Float>, Spinnable<MaskedNumberFieldSpinner> {
  private final SpinnableMixin spinnableMixin;
  private Float step = null;

  MaskedNumberFieldSpinner(SpinnableMixin spinnableMixin) {
    super();
    this.spinnableMixin = spinnableMixin;
  }

  /**
   * Constructs a new masked number field spinner.
   */
  public MaskedNumberFieldSpinner() {
    super();
    this.spinnableMixin = new SpinnableMixin(this);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberFieldSpinner setStep(Float step) {
    Objects.requireNonNull(step, "Step cannot be null");
    this.step = step;
    setUnrestrictedProperty("step", step);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Float getStep() {
    return step;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberFieldSpinner spinUp() {
    spinnableMixin.spinUp();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberFieldSpinner spinDown() {
    spinnableMixin.spinDown();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputNSpinner(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputNSpinner", e);
    }
  }
}
