package org.dwcj.component.numberfield.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.numberfield.NumberField;

public class NumberFieldModifyEvent implements ComponentEvent {

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
