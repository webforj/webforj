package com.webforj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import java.lang.StackWalker.Option;
import java.lang.StackWalker.StackFrame;
import java.util.Set;

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
  // Reflection frames stay visible, so a reflective call is judged by the reflection frame that
  // invoked the accessor, the frame a full stack trace reports at the same depth.
  private static final StackWalker STACK_WALKER =
      StackWalker.getInstance(Set.of(Option.SHOW_REFLECT_FRAMES));

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjWindow getBBjWindow(Window window) throws IllegalAccessException {
    String caller = STACK_WALKER.walk(frames -> frames.skip(1).findFirst())
        .map(StackFrame::getClassName).orElse("");
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
