package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.component.Component;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.LegacyDwcComponent;
import org.dwcj.component.window.Window;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel
 * class it's not for customer use, only for use in the "friend" classes Pattern see Tulach, p.75ff
 */
public abstract class ComponentAccessor {
  private static ComponentAccessor accessor;

  protected ComponentAccessor() {}

  /**
   * Factory to obtain the instance of the accessor.
   *
   * @return - the accessor
   */
  public static ComponentAccessor getDefault() {
    ComponentAccessor a = accessor;
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
   * Set the accessor instance into the static field.
   *
   * @param accessor - the accessor instance
   */
  public static void setDefault(ComponentAccessor accessor) {
    if (ComponentAccessor.accessor != null) {
      throw new IllegalStateException();
    }
    ComponentAccessor.accessor = accessor;
  }

  /**
   * Gets the BBjControl under the DwcComponent.
   *
   * @param ctrl - get the BBjControl under the DwcComponent
   * @return - the BBjControl
   * @throws IllegalAccessException The exception thrown on illegal access.
   * @deprecated Use {@link #getControl(DwcComponent)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public abstract BBjControl getBBjControl(LegacyDwcComponent ctrl) throws IllegalAccessException;

  /**
   * Gets the BBjControl under the passed DwcComponent.
   *
   * @param component the component to get the BBjControl from.
   *
   * @return the BBjControl.
   * @throws IllegalAccessException The exception thrown on illegal access.
   */
  public abstract BBjControl getControl(DwcComponent<?> component) throws IllegalAccessException;

  public abstract void create(Component ctrl, Window panel) throws IllegalAccessException;
}

