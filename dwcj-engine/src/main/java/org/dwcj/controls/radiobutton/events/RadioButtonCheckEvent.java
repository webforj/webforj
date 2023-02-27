package org.dwcj.controls.radiobutton.events;

import org.dwcj.controls.radiobutton.RadioButton;
import org.dwcj.interfaces.ControlEvent;

public final class RadioButtonCheckEvent implements ControlEvent {
    
    private final RadioButton control;

    private boolean isChecked = false; 

    public RadioButtonCheckEvent(RadioButton rButton, boolean checked) {
        this.isChecked = checked;
        this.control = rButton;
    }

    public boolean isChecked(){
        return isChecked;
    }

    @Override
    public RadioButton getControl() { return control; }

}
