package org.dwcj.component.event.sink;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The sink is responsible for setting and removing the callback on a BBjControl.
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public interface EventSinkInterface {

  /**
   * Set a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be set.
   */
  public void setCallback();

  /**
   * Remove a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be removed.
   */
  public void removeCallback();

  /**
   * Get the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  public EventDispatcher getEventDispatcher();
}
