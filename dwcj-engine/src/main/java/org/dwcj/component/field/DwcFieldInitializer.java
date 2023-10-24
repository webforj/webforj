package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.FocusableDwcComponent;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasReadOnly;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * A {@link DwcField} that is used to initialize a {@link BBjEditBox} control.
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the field.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
abstract class DwcFieldInitializer<T extends FocusableDwcComponent<T> & HasReadOnly<T>, V>
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
      throw new DwcjRuntimeException("Failed to create BBjEditBox", e);
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
      throw new DwcjRuntimeException(e);
    }
  }
}
