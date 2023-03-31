package org.dwcj.component.textfield.event;

import org.dwcj.component.textfield.TextField;
import org.dwcj.interfaces.ControlEvent;

public class StringEditBoxEditModifyEvent implements ControlEvent {

    private final TextField control;

    public StringEditBoxEditModifyEvent(TextField stringEditBox){
        this.control = stringEditBox;
    }

    @Override
    public TextField getControl() {
        return control;
    }

    public String toString() {
        return "Event: StringEditBox modified";
    }
    
}