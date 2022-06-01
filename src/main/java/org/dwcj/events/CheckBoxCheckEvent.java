package org.dwcj.events;

import org.dwcj.controls.CheckBox;

public final class CheckBoxCheckEvent implements IDwcEvent {

    private final CheckBox control;

    public CheckBoxCheckEvent(CheckBox cCheckBox) {
        this.control = cCheckBox;
    }

    public boolean isSelected(){
        return control.isSelected();
    }

    @Override
    public CheckBox getControl() { return control; }
}
