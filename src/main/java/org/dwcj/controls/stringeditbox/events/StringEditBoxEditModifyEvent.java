package org.dwcj.controls.stringeditbox.events;

import org.dwcj.controls.stringeditbox.StringEditBox;
import org.dwcj.interfaces.ControlEvent;

public class StringEditBoxEditModifyEvent implements ControlEvent {

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