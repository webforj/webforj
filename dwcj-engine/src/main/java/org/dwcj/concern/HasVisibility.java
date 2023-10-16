package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for implementing methods to control a component's visibility on a page.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasVisibility<T extends Component> {

  /**
   * Checks whether the component is visible or invisible.
   *
   * @return true if the component is visible, false if it's invisible.
   */
  public boolean isVisible();

  /**
   * Sets whether the component is visible or invisible.
   *
   * @param visible true to make the component visible, false to make it invisible.
   * @return the component itself.
   */
  public T setVisible(boolean visible);
}
