package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for managing focus behavior of components.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @see HasTabFocus
 * @see HasTabIndex
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasFocus<T extends Component> {

  /**
   * Gives focus to the component when it is added to a window.
   *
   * @return The component itself.
   */
  public T focus();

  /**
   * Checks whether the user can focus to the component using the tab key.
   *
   * <p>
   * It's important to note that if the component is set as focusable but is disabled, the user
   * won't be able to navigate to the component using the Tab key or focus it programmatically.
   * However, the component will still be included in the tab order.
   * </p>
   *
   * @return true if the user can navigate to the component with the Tab key, false if not.
   */
  public boolean isFocusable();

  /**
   * Sets whether the user can focus to the component using the tab key.
   *
   * <p>
   * This method allows you to enable or disable focusing the component. When enabled, the component
   * will be part of the tab order. When disabled the component is excluded from tab navigation.
   * </p>
   *
   * @param focusable true to enable focusing the component, false to disable it.
   * @return the component itself.
   */
  public T setFocusable(boolean focusable);
}
