package org.dwcj.events;

import org.dwcj.controls.StringEditBox;

public class StringEditBoxEditModifyEvent implements IDwcEvent {

    private final StringEditBox control;

    public StringEditBoxEditModifyEvent(StringEditBox stringEditBox){
        this.control = stringEditBox;
    }

    @Override
    public StringEditBox getControl() {
        return control;
    }

    public String toString() {
        return "Event: StringEditBox modified";
    }
    
}