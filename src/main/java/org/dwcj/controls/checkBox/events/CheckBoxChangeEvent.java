package org.dwcj.controls.checkBox.events;

import org.dwcj.controls.checkBox.CheckBox;
import org.dwcj.interfaces.IDwcEvent;

public final class CheckBoxChangeEvent implements IDwcEvent {

    private final CheckBox control;

    private boolean isChecked = false; 

    public CheckBoxChangeEvent(CheckBox cCheckBox, boolean checked) {
        this.isChecked = checked;
        this.control = cCheckBox;
    }

    public boolean isChecked(){
        return isChecked;
    }

    @Override
    public CheckBox getControl() { return control; }
}
