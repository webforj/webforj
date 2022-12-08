package org.dwcj.controls.dateEditBox.events;

import org.dwcj.controls.dateEditBox.DateEditBox;
import org.dwcj.interfaces.IDwcEvent;

public final class DateEditBoxEditModifyEvent implements IDwcEvent {
    private final DateEditBox control;

    public DateEditBoxEditModifyEvent(DateEditBox cDateEditBox) {
        this.control = cDateEditBox;
    }

    @Override
    public DateEditBox getControl() {
        return control;
    }

    public String toString() {
        return "Event: DateEditBox modified";
    }
}
