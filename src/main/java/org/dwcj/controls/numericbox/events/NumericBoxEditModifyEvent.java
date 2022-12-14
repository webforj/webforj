package org.dwcj.controls.numericbox.events;

import org.dwcj.controls.numericbox.NumericBox;
import org.dwcj.interfaces.DwcEvent;

public class NumericBoxEditModifyEvent implements DwcEvent {

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
