package org.dwcj.component.numericbox.events;

import org.dwcj.component.numericbox.NumericBox;
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
