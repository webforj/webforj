package com.webforj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;

/**
 * Implementation of the WindowAccessor that provides low-level access to BBj Window for DWCj
 * Windows. This class ensures that only authorized callers from within the com.webforj package can
 * access the underlying BBjControl.
 *
 * @author Stephan Wald
 * @author Hyyan Abo Fakher
 *
 * @see WindowAccessor
 * @see ComponentAccessor
 */
final class WindowAccessorImpl extends WindowAccessor {

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjWindow getBBjWindow(Window window) throws IllegalAccessException {
    StackTraceElement[] stack = Thread.currentThread().getStackTrace();
    String caller = stack[2].getClassName();
    if (!caller.startsWith("com.webforj.")) {
      throw new IllegalAccessException(
          String.format("The class '%s' attempted unauthorized access to BBj Window. "
              + "Access is restricted to classes within the 'com.webforj' package hierarchy. "
              + "Ensure that any class interacting with BBj Window adheres "
              + "to this access control policy.", caller));
    }

    return window.getBbjWindow();
  }
}
