package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Implementation of the ComponentAccessor that provides low-level access to BBj Controls for DWC
 * components. This class ensures that only authorized callers from within the com.webforj package
 * can access the underlying BBjControl.
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

    if (component == null) {
      throw new NullPointerException("Component cannot be null.");
    }

    try {
      Method create = Component.class.getDeclaredMethod("create", Window.class);
      create.setAccessible(true); // NOSONAR
      create.invoke(component, window);

      if (component instanceof DwcComponent) {
        BBjControl control = getControl(component);
        if (control != null) {
          control.setUserData(component);
        }
      }
    } catch (InvocationTargetException | NoSuchMethodException | BBjException e) {
      throw new WebforjRuntimeException(
          String.format("Failed to create component '%s'.", component.getClass().getName()), e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjControl getControl(Component component) throws IllegalAccessException {
    verifyCaller();

    if (component == null) {
      throw new NullPointerException("Component cannot be null.");
    }

    try {
      if (component instanceof LegacyDwcComponent lc) {
        return lc.getControl();
      } else if (component instanceof DwcComponent) {
        return ((DwcComponent<?>) component).getControl();
      } else if (component instanceof Composite) {
        Method getBoundComponent = Composite.class.getDeclaredMethod("getBoundComponent");
        getBoundComponent.setAccessible(true); // NOSONAR
        Object boundComponent = getBoundComponent.invoke(component);

        return getControl((Component) boundComponent);
      } else {
        throw new IllegalAccessException(
            "Cannot find the underlying BBjControl for the given component: "
                + component.getClass().getName());
      }
    } catch (NoSuchMethodException e) {
      throw new IllegalAccessException("The Composite " + component.getClass().getName()
          + " does not implement the 'initBoundComponent' method.");
    } catch (SecurityException | IllegalArgumentException | InvocationTargetException e) {
      throw new IllegalAccessException(
          "Failed to access 'initBoundComponent' method for Composite component.");
    }
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
    if (!caller.startsWith("com.webforj.")) {
      throw new IllegalAccessException(
          String.format("The class '%s' attempted unauthorized access to BBj Control. "
              + "Access is restricted to classes within the 'com.webforj' package hierarchy. "
              + "Ensure that any class interacting with BBj Control adheres "
              + "to this access control policy.", caller));
    }
  }
}
