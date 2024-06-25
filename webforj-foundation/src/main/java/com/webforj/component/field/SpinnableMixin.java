package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjSpinner;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Represents a mixin for a dwc field that can be spun in a spinner.
 *
 * @param <T> The type of the field itself.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
class SpinnableMixin implements Spinnable<SpinnableMixin> {
  private final DwcField<?, ?> field;

  SpinnableMixin(DwcField<?, ?> field) {
    this.field = field;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SpinnableMixin spinUp() {
    spin(true);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SpinnableMixin spinDown() {
    spin(false);
    return this;
  }

  void spin(boolean up) {
    BBjSpinner spinner = inferSpinner();

    if (spinner != null) {
      spinner.spin(up);
    } else {
      if (field instanceof HasOptions) {
        HasOptions<?> hasOptions = (HasOptions<?>) field;
        hasOptions.setOptionIndex(hasOptions.getOptionIndex() + (up ? 1 : -1));
      }
    }
  }

  BBjSpinner inferSpinner() {
    try {
      return (BBjSpinner) ComponentAccessor.getDefault().getControl(field);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
