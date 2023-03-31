package org.dwcj.component.field.events;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.field.TextBox;

public class TextBoxEditModifyEvent implements ComponentEvent {

    private final TextBox control;

    public TextBoxEditModifyEvent(TextBox tBox){
        this.control = tBox;
    }

    @Override
    public TextBox getControl() {
        return control;
    }

    public String toString() {
        return "Event: TextBox modified";
    }
    
}