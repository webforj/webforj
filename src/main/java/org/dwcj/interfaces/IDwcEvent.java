package org.dwcj.interfaces;

import org.dwcj.controls.AbstractDwcControl;

/**
 * Base interface for DWC events
 */
public interface IDwcEvent {
    /**
     * obtain a reference to the control that triggered the event
     *
     * @return the control that triggered the event
     */
    AbstractDwcControl getControl();
}
