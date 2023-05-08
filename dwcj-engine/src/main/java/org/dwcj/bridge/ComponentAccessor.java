package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;

import org.dwcj.Environment;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel
 * class it's not for customer use, only for use in the "friend" classes Pattern see Tulach, p.75ff
 */
public abstract class ComponentAccessor {
  private static ComponentAccessor accessor;

  protected ComponentAccessor() {}

  /**
   * factory to obtain the instance of the accessor.
   *
   * @return - the accessor
   */
  public static ComponentAccessor getDefault() {
    ComponentAccessor a = accessor;
    if (a != null) {
      return a;
    }
    try {
      Class.forName(AbstractWindow.class.getName(), true, AbstractWindow.class.getClassLoader());
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
   * @param ctrl - get the BBjControl under the AbstractDwcComponent.
   * @return - the BBjControl
   */
  public abstract BBjControl getBBjControl(AbstractDwcComponent ctrl);

  /**
   *  Sets the BBjControl under the AbstractDwcComponent.
   *
   * @param component the component to access.
   * @param ctrl the new control.
   * @throws IllegalAccessException
   */
  public abstract void setBBjControl(AbstractDwcComponent component, BBjControl ctrl)
      throws IllegalAccessException;

  public abstract void create(AbstractComponent ctrl, AbstractWindow panel)
      throws IllegalAccessException;
}

