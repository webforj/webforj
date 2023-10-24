package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for controlling the enabled state of a component after it has been rendered on the
 * page.
 *
 * <p>
 * This interface provides methods to query and modify the enabled state of a component, allowing
 * you to determine whether it is currently enabled and set its enablement status.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasEnablement<T extends Component> {

  /**
   * Checks if the component is currently enabled.
   *
   * @return true if the component is enabled, false if it is disabled
   */
  public boolean isEnabled();

  /**
   * Sets the enablement state of the component.
   *
   * @param enabled true to enable the component, false to disable it
   * @return the component itself.
   */
  public T setEnabled(boolean enabled);
}
