package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcValidatableComponent;
import com.webforj.component.window.Window;
import com.webforj.concern.HasReadOnly;
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

  protected DwcFieldInitializer() {
    super();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
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
