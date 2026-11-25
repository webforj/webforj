package org.example.accessor;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.LegacyDwcComponent;
import com.webforj.component.window.Window;

/**
 * Calls the webforJ accessors from a class outside the {@code com.webforj} package hierarchy.
 */
public final class ForeignAccessorCaller {

  private ForeignAccessorCaller() {}

  /**
   * Creates the component through the given accessor.
   *
   * @param accessor the accessor to call
   * @param component the component to create
   * @param window the window to create the component in
   * @throws IllegalAccessException if the accessor refuses the call
   */
  public static void create(ComponentAccessor accessor, Component component, Window window)
      throws IllegalAccessException {
    accessor.create(component, window);
  }

  /**
   * Gets the control of the component through the given accessor.
   *
   * @param accessor the accessor to call
   * @param component the component to get the control of
   * @return the control the accessor returned
   * @throws IllegalAccessException if the accessor refuses the call
   */
  public static BBjControl getControl(ComponentAccessor accessor, Component component)
      throws IllegalAccessException {
    return accessor.getControl(component);
  }

  /**
   * Gets the control of the legacy component through the given accessor.
   *
   * @param accessor the accessor to call
   * @param component the legacy component to get the control of
   * @return the control the accessor returned
   * @throws IllegalAccessException if the accessor refuses the call
   */
  @SuppressWarnings("removal")
  public static BBjControl getBBjControl(ComponentAccessor accessor, LegacyDwcComponent component)
      throws IllegalAccessException {
    return accessor.getBBjControl(component);
  }

  /**
   * Gets the BBj window of the window through the given accessor.
   *
   * @param accessor the accessor to call
   * @param window the window to get the BBj window of
   * @return the BBj window the accessor returned
   * @throws IllegalAccessException if the accessor refuses the call
   */
  public static BBjWindow getBBjWindow(WindowAccessor accessor, Window window)
      throws IllegalAccessException {
    return accessor.getBBjWindow(window);
  }
}
