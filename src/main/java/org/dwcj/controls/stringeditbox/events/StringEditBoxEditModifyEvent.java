package org.dwcj.controls.stringeditbox.events;

import org.dwcj.controls.stringeditbox.StringEditBox;
import org.dwcj.interfaces.DwcEvent;

public class StringEditBoxEditModifyEvent implements DwcEvent {

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