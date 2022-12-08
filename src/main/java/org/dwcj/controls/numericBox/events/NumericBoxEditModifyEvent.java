package org.dwcj.controls.numericBox.events;

import org.dwcj.controls.numericBox.NumericBox;
import org.dwcj.interfaces.IDwcEvent;

public class NumericBoxEditModifyEvent implements IDwcEvent {

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
