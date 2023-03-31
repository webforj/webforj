package org.dwcj.component.numberfield.events;

import org.dwcj.component.numberfield.NumericBox;
import org.dwcj.interfaces.ControlEvent;

public class NumericBoxEditModifyEvent implements ControlEvent {

    private final NumericBox control;

    public NumericBoxEditModifyEvent(NumericBox nBox){
        this.control = nBox;
    }

    @Override
    public NumericBox getControl() {
        return control;
    }

    public String toString() {
        return "Event: DateEditBox modified";
    }
    
}
