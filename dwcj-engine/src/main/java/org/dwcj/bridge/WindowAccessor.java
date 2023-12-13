package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.component.window.Window;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Serves as a bridge for accessing BBj-specific components from the DWCj environment. It provides
 * access to the BBjWindow object behind the DWCj Panel instances. This class should not be used
 * directly by consumers and is intended only for internal use following the "friend" classes.
 *
 * @author Stephan Wald
 */
public abstract class WindowAccessor {
  private static WindowAccessor accessor;

  /**
   * Constructs a WindowAccessor. Intended for internal use only.
   */
  protected WindowAccessor() {}

  /**
   * Provides access to the singleton instance of the WindowAccessor, initializing it if necessary
   * by loading the Window class to trigger the static initializer that sets the accessor.
   *
   * @return The singleton instance of the WindowAccessor.
   */
  public static WindowAccessor getDefault() {
    WindowAccessor a = accessor;
    if (a != null) {
      return a;
    }
    try {
      Class.forName(Window.class.getName(), true, Window.class.getClassLoader());
    } catch (Exception e) {
      throw new DwcjRuntimeException("Unable to load Window class.", e);
    }

    return accessor;
  }

  /**
   * Sets the singleton instance of the WindowAccessor. This should only be called once and is
   * protected to prevent external modification.
   *
   * @param accessor The instance of ComponentAccessor to set.
   * @throws IllegalStateException If an accessor has already been set.
   */
  public static void setDefault(WindowAccessor accessor) {
    if (WindowAccessor.accessor != null) {
      throw new IllegalStateException("WindowAccessor already set and cannot be redefined");
    }

    WindowAccessor.accessor = accessor;
  }

  /**
   * Retrieves the {@code BBjWindow} for the specified {@code Window}.
   *
   * @param window The {@code window} for which to get the {@code BBjWindow}.
   * @return The associated {@code BBjWindow} instance.
   *
   * @throws IllegalAccessException If access to the {@code BBjWindow} is not allowed.
   */
  public abstract BBjWindow getBBjWindow(Window window) throws IllegalAccessException;

}

