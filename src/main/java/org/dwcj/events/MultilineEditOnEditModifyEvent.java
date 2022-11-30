package org.dwcj.events;

import org.dwcj.controls.MultilineEdit;

public final class MultilineEditOnEditModifyEvent implements IDwcEvent {
    private final MultilineEdit control;

    public MultilineEditOnEditModifyEvent(MultilineEdit cMultilineEdit) {
        this.control = cMultilineEdit;
    }

    @Override
    public MultilineEdit getControl() {
        return control;
    }

    public String toString() {
        return "Event: MultilineEdit modified";
    }
}
