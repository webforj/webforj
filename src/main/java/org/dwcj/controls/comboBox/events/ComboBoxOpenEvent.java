package org.dwcj.controls.comboBox.events;

import org.dwcj.controls.comboBox.ComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class ComboBoxOpenEvent implements IDwcEvent{
    
    private final ComboBox control;

    public ComboBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
