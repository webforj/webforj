package org.dwcj.events;

import org.dwcj.controls.CheckBox;

public final class CheckBoxCheckEvent implements IDwcEvent {

    private final CheckBox control;

    private boolean isChecked = false; 

    public CheckBoxCheckEvent(CheckBox cCheckBox, boolean checked) {
        this.isChecked = checked;
        this.control = cCheckBox;
    }

    public boolean isSelected(){
        return isChecked;
    }

    @Override
    public CheckBox getControl() { return control; }
}
