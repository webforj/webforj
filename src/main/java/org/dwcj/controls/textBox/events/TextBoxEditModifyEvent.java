package org.dwcj.controls.textBox.events;

import org.dwcj.controls.textBox.TextBox;
import org.dwcj.interfaces.IDwcEvent;

public class TextBoxEditModifyEvent implements IDwcEvent {

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