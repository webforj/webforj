package org.dwcj.component;

/**
 * Base interface for DWC control events
 */
public interface ComponentEvent {
  /**
   * obtain a reference to the control that triggered the event
   *
   * @return the control that triggered the event
   */
  AbstractComponent getControl();
}
