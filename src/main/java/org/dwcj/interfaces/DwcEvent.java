package org.dwcj.interfaces;

import org.dwcj.controls.AbstractControl;

/**
 * Base interface for DWC events
 */
public interface DwcEvent {
    /**
     * obtain a reference to the control that triggered the event
     *
     * @return the control that triggered the event
     */
    AbstractControl getControl();
}
