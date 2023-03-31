package org.dwcj.interfaces;

import org.dwcj.component.AbstractComponent;

/**
 * Base interface for DWC control events
 */
public interface ControlEvent {
    /**
     * obtain a reference to the control that triggered the event
     *
     * @return the control that triggered the event
     */
    AbstractComponent getControl();
}
