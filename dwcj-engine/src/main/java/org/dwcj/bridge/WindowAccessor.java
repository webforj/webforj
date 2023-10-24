package org.dwcj.bridge;

import org.dwcj.Environment;
import org.dwcj.component.window.Window;

import com.basis.bbj.proxies.sysgui.BBjWindow;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel
 * class it's not for customer use, only for use in the "friend" classes Pattern see Tulach, p.75ff
 */
public abstract class WindowAccessor {
  private static WindowAccessor accessor;

  protected WindowAccessor() {}

  /**
   * Gets the accessor instance to access the protected methods in the DWCJ Panel instances
   *
   * @return the accessor instance
   */
  public static WindowAccessor getDefault() {
    WindowAccessor a = accessor;
    if (a != null) {
      return a;
    }
    try {
      Class.forName(Window.class.getName(), true, Window.class.getClassLoader());
    } catch (Exception e) {
      Environment.logError(e);
    }
    return accessor;
  }

  /**
   * Sets the accessor instance for static access
   *
   * @param accessor the instance of the accessor implementation
   */
  public static void setDefault(WindowAccessor accessor) {
    if (WindowAccessor.accessor != null) {
      throw new IllegalStateException();
    }
    WindowAccessor.accessor = accessor;
  }


  /**
   *
   * @param panel the panel that contains the BBj window
   * @return the BBjWindow object behind the panel
   * @throws IllegalAccessException
   */
  public abstract BBjWindow getBBjWindow(Window panel) throws IllegalAccessException;

}

