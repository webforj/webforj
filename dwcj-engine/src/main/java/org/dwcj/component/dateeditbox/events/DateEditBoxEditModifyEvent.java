package org.dwcj.component.dateeditbox.events;

import org.dwcj.component.dateeditbox.DateEditBox;
import org.dwcj.interfaces.ControlEvent;

public final class DateEditBoxEditModifyEvent implements ControlEvent {
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
