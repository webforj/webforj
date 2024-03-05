package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface for implementing methods that allow toggling the read-only status on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasReadOnly<T extends Component> {

  /**
   * Sets whether a user can edit the component.
   *
   * @param readOnly true to disable editing, false to enable editing.
   * @return the component itself after configuring the read-only status.
   */
  T setReadOnly(boolean readOnly);

  /**
   * Checks whether the component is set to read-only.
   *
   * @return true if the user cannot edit the component, false if editing is allowed.
   */
  boolean isReadOnly();
}
