package org.dwcj.controls.textarea.events;

import org.dwcj.controls.textarea.TextArea;
import org.dwcj.interfaces.IDwcEvent;

public final class TextAreaOnEditModifyEvent implements IDwcEvent {
    private final TextArea control;

    public TextAreaOnEditModifyEvent(TextArea cMultilineEdit) {
        this.control = cMultilineEdit;
    }

    @Override
    public TextArea getControl() {
        return control;
    }

    public String toString() {
        return "Event: MultilineEdit modified";
    }
}
