package org.dwcj.events;

import org.dwcj.controls.AbstractDwcControl;

public interface IDwcEvent {
    /**
     * obtain a reference to the control that triggered the event
     *
     * @return the control that triggered the event
     */
    AbstractDwcControl getControl();
}
