package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.component.Component;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.LegacyDwcComponent;
import org.dwcj.component.window.Window;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Serves as a bridge for accessing BBj-specific components from the DwcJ environment. It provides
 * an interface for internal operations that require direct manipulation of BBj controls that back
 * the DWC components. This class should not be used directly by consumers and is intended only for
 * internal use following the "friend" classes.
 *
 * @author Stephan Wald
 * @author Hyyan Abo Fakher
 */
public abstract class ComponentAccessor {
  private static ComponentAccessor accessor;

  /**
   * Constructs a ComponentAccessor. Intended for internal use only.
   */
  protected ComponentAccessor() {}

  /**
   * Provides access to the singleton instance of the ComponentAccessor, initializing it if
   * necessary by loading the Window class to trigger the static initializer that sets the accessor.
   *
   * @return The singleton instance of the ComponentAccessor.
   */
  public static ComponentAccessor getDefault() {
    ComponentAccessor a = accessor;
    if (a != null) {
      return a;
    }

    try {
      Class.forName(Window.class.getName(), true, Window.class.getClassLoader());
    } catch (ClassNotFoundException e) {
      throw new DwcjRuntimeException("Unable to load Window class.", e);
    }

    return accessor;
  }

  /**
   * Sets the singleton instance of the ComponentAccessor. This should only be called once and is
   * protected to prevent external modification.
   *
   * @param accessor The instance of ComponentAccessor to set.
   * @throws IllegalStateException If an accessor has already been set.
   */
  public static void setDefault(ComponentAccessor accessor) {
    if (ComponentAccessor.accessor != null) {
      throw new IllegalStateException("ComponentAccessor already set and cannot be redefined");
    }

    ComponentAccessor.accessor = accessor;
  }

  /**
   * Retrieves the underlying BBjControl for a LegacyDwcComponent. This method is deprecated and
   * should no longer be used.
   *
   * @param component The LegacyDwcComponent for which to retrieve the BBjControl.
   * @return The underlying BBjControl instance.
   * @throws IllegalAccessException If access to the control is not allowed.
   * @deprecated As of version 23.05, replaced by {@link #getControl(DwcComponent)} and scheduled
   *             for removal in future releases.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public abstract BBjControl getBBjControl(LegacyDwcComponent component)
      throws IllegalAccessException;

  /**
   * Retrieves the BBjControl for the specified DwcComponent.
   *
   * @param component The DwcComponent for which to get the BBjControl.
   * @return The associated BBjControl instance.
   * @throws IllegalAccessException If access to the control is not allowed.
   */
  public abstract BBjControl getControl(DwcComponent<?> component) throws IllegalAccessException;

  /**
   * Creates a DWC component within the context of the specified window.
   *
   * @param component The DWC component to create.
   * @param window The window within which the component is to be created.
   * @throws IllegalAccessException If the creation within the window is not permitted.
   */
  public abstract void create(Component component, Window window) throws IllegalAccessException;
}
