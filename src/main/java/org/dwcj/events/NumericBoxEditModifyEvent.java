package org.dwcj.events;

import org.dwcj.controls.NumericBox;

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
