package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import java.lang.reflect.Method;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.window.Window;


/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel
 * class Pattern see Tulach, p.75ff
 */
final class ComponentAccessorImpl extends ComponentAccessor {

  public static final String YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD =
      ": You're not allowed to access this method!";

  @Override
  public BBjControl getBBjControl(LegacyDwcComponent ctrl) throws IllegalAccessException {

    StackTraceElement[] stack = Thread.currentThread().getStackTrace();
    String caller = stack[2].getClassName();
    if (caller.startsWith("org.dwcj.")) {
      return ctrl.getControl();
    }
    App.consoleLog(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
    throw new IllegalAccessException(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjControl getControl(DwcComponent<?> component) throws IllegalAccessException {
    StackTraceElement[] stack = Thread.currentThread().getStackTrace();
    String caller = stack[2].getClassName();
    if (caller.startsWith("org.dwcj.")) {
      return component.getControl();
    }
    throw new IllegalAccessException(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
  }

  @Override
  @SuppressWarnings("java:S3011") // allow increasing acessibility
  public void create(Component ctrl, Window panel) throws IllegalAccessException {

    StackTraceElement[] stack = Thread.currentThread().getStackTrace();
    String caller = stack[2].getClassName();

    if (caller.startsWith("org.dwcj.")) {
      try {
        boolean found = false;
        Class<?> clazz = ctrl.getClass();
        while (clazz != null && !found) {
          Method[] methods = clazz.getDeclaredMethods();
          for (Method method : methods) {
            if (method.getName().equals("create") && method.getParameterCount() == 1
                && method.getParameterTypes()[0]
                    .equals(Class.forName("org.dwcj.component.window.Window"))) {
              method.setAccessible(true);
              method.invoke(ctrl, panel);
              found = true;
              break;
            }
          }

          if (!found) {
            clazz = clazz.getSuperclass();
          }
        }
      } catch (Exception e) {
        Environment.logError(e);
      }
      return;
    }

    App.consoleLog(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
    throw new IllegalAccessException(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
  }
}
