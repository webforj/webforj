package org.dwcj.events;

import org.dwcj.controls.RadioButton;

public final class RadioButtonCheckEvent implements IDwcEvent {
    
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
