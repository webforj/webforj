package org.dwcj.component.checkbox.event;

import org.dwcj.component.checkbox.CheckBox;
import org.dwcj.interfaces.ControlEvent;

public final class CheckBoxChangeEvent implements ControlEvent {

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
