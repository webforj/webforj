package org.dwcj.component.numberfield.event;

import org.dwcj.component.numberfield.NumberField;
import org.dwcj.interfaces.ControlEvent;

public class NumericBoxEditModifyEvent implements ControlEvent {

    private final NumberField control;

    public NumericBoxEditModifyEvent(NumberField nBox){
        this.control = nBox;
    }

    @Override
    public NumberField getControl() {
        return control;
    }

    public String toString() {
        return "Event: DateEditBox modified";
    }
    
}
