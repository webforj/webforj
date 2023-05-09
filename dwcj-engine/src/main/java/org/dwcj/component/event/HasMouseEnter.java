package org.dwcj.component.event;

import org.dwcj.component.AbstractComponent;

/** Interface facilitating implementing a MouseEnter event for a component. */
public interface HasMouseEnter {

  /**
   * Adds a MouseEnter event for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public AbstractComponent addMouseEnterListener(EventListener<MouseEnterEvent> listener);

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see HasMouseEnter#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The component itself
   */
  public AbstractComponent onMouseEnter(EventListener<MouseEnterEvent> listener);

  /**
   * Removes a MouseEnter event from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public AbstractComponent removeMouseEnterListener(EventListener<MouseEnterEvent> listener);

}
