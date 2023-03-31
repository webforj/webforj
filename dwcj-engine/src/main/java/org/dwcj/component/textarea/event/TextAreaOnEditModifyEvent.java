package org.dwcj.component.textarea.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.textarea.TextArea;

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
