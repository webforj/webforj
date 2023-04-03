package org.dwcj.component.datefield.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.datefield.DateField;

public final class DateFieldModifyEvent implements ComponentEvent {
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
