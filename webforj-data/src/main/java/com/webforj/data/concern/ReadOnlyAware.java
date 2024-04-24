package com.webforj.data.concern;


/**
 * An interface for implementing methods that allow toggling the read-only status on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface ReadOnlyAware<T> {

  /**
   * Sets whether a user can edit the component.
   *
   * @param readOnly true to disable editing, false to enable editing.
   * @return the component itself after configuring the read-only status.
   */
  public T setReadOnly(boolean readOnly);

  /**
   * Checks whether the component is set to read-only.
   *
   * @return true if the user cannot edit the component, false if editing is allowed.
   */
  public boolean isReadOnly();
}
