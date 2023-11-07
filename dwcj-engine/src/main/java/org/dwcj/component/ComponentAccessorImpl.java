package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.window.Window;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Implementation of the ComponentAccessor that provides low-level access to BBj Controls for DWC
 * components. This class ensures that only authorized callers from within the org.dwcj package can
 * access the underlying BBjControl.
 *
 * @author Stephan Wald
 * @author Hyyan Abo Fakher
 *
 * @see ComponentAccessor
 */
final class ComponentAccessorImpl extends ComponentAccessor {

  /**
   * {@inheritDoc}
   */
  @Override
  public void create(Component component, Window window) throws IllegalAccessException {
    verifyCaller();

    try {
      Method create = Component.class.getDeclaredMethod("create", Window.class);
      create.setAccessible(true); // NOSONAR
      create.invoke(component, window);
    } catch (InvocationTargetException | NoSuchMethodException e) {
      throw new DwcjRuntimeException(
          String.format("Failed to create component '%s'.", component.getClass().getName()), e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjControl getControl(DwcComponent<?> component) throws IllegalAccessException {
    verifyCaller();
    return component.getControl();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjControl getBBjControl(LegacyDwcComponent ctrl) throws IllegalAccessException {
    verifyCaller();
    return ctrl.getControl();
  }

  private void verifyCaller() throws IllegalAccessException {
    StackTraceElement[] stack = Thread.currentThread().getStackTrace();
    String caller = stack[2].getClassName();
    if (!caller.startsWith("org.dwcj.")) {
      throw new IllegalAccessException(
          String.format("The class '%s' attempted unauthorized access to BBj Control. "
              + "Access is restricted to classes within the 'org.dwcj' package hierarchy. "
              + "Ensure that any class interacting with BBj Control adheres "
              + "to this access control policy.", caller));
    }
  }
}
