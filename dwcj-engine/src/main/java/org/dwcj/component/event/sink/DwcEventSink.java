package org.dwcj.component.event.sink;

import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The sink is responsible for setting and removing the callback on a BBjControl.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface DwcEventSink {

  /**
   * Sets a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be set.
   */
  public void setCallback();

  /**
   * Removes a callback on an underlying BBj control.
   *
   * @throws DwcjRuntimeException if the callback cannot be removed.
   */
  public void removeCallback();

  /**
   * Gets the event dispatcher instance.
   *
   * @return the event dispatcher instance.
   */
  public EventDispatcher getEventDispatcher();
}
