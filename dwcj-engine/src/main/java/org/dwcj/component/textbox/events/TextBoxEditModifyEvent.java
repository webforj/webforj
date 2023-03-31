package org.dwcj.component.textbox.events;

import org.dwcj.component.textbox.TextBox;
import org.dwcj.interfaces.ControlEvent;

public class TextBoxEditModifyEvent implements ControlEvent {

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