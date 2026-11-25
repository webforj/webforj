package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.StackWalker.Option;
import java.lang.StackWalker.StackFrame;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;

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
  private static final StackWalker STACK_WALKER =
      StackWalker.getInstance(Set.of(Option.SHOW_REFLECT_FRAMES));
  private static final Method CREATE_METHOD = findMethod(Component.class, "create", Window.class);
  private static final Method GET_BOUND_COMPONENT_METHOD =
      findMethod(Composite.class, "getBoundComponent");

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
      CREATE_METHOD.invoke(component, window);

      if (component instanceof DwcComponent) {
        BBjControl control = getControl(component);
        if (control != null) {
          control.setUserData(component);
        }
      }
    } catch (InvocationTargetException | BBjException e) {
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
        Object boundComponent = GET_BOUND_COMPONENT_METHOD.invoke(component);

        return getControl((Component) boundComponent);
      } else {
        throw new IllegalAccessException(
            "Cannot find the underlying BBjControl for the given component: "
                + component.getClass().getName());
      }
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

  private static void verifyCaller() throws IllegalAccessException {
    String caller = STACK_WALKER.walk(frames -> frames.skip(2).findFirst())
        .map(StackFrame::getClassName).orElse("");
    if (!caller.startsWith("com.webforj.")) {
      throw new IllegalAccessException(
          String.format("The class '%s' attempted unauthorized access to BBj Control. "
              + "Access is restricted to classes within the 'com.webforj' package hierarchy. "
              + "Ensure that any class interacting with BBj Control adheres "
              + "to this access control policy.", caller));
    }
  }

  private static Method findMethod(Class<?> type, String name, Class<?>... parameterTypes) {
    try {
      Method method = type.getDeclaredMethod(name, parameterTypes);
      method.setAccessible(true); // NOSONAR

      return method;
    } catch (NoSuchMethodException e) {
      throw new IllegalStateException(
          String.format("Method '%s' not found in '%s'.", name, type.getName()), e);
    }
  }
}
