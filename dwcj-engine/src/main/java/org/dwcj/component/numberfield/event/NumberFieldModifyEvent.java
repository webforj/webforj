package org.dwcj.component.numberfield.event;

import org.dwcj.component.numberfield.NumberField;
import org.dwcj.interfaces.ControlEvent;

public class NumberFieldModifyEvent implements ControlEvent {

    private final NumberField control;

    public NumberFieldModifyEvent(NumberField nBox){
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
