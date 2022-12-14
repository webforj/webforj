package org.dwcj.controls.textbox.events;

import org.dwcj.controls.textbox.TextBox;
import org.dwcj.interfaces.DwcEvent;

public class TextBoxEditModifyEvent implements DwcEvent {

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