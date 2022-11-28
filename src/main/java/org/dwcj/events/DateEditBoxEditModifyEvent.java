package org.dwcj.events;

import org.dwcj.controls.DateEditBox;

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
