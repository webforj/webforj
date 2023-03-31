package org.dwcj.component.datefield.event;

import org.dwcj.component.datefield.DateField;
import org.dwcj.interfaces.ControlEvent;

public final class DateFieldModifyEvent implements ControlEvent {
    private final DateField control;

    public DateFieldModifyEvent(DateField cDateEditBox) {
        this.control = cDateEditBox;
    }

    @Override
    public DateField getControl() {
        return control;
    }

    public String toString() {
        return "Event: DateEditBox modified";
    }
}
