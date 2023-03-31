package org.dwcj.component.textarea.events;

import org.dwcj.component.textarea.TextArea;
import org.dwcj.interfaces.ComponentEvent;

public final class TextAreaOnEditModifyEvent implements ComponentEvent {
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
