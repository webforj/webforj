package org.dwcj.controls.comboBox.events;

import org.dwcj.controls.comboBox.ComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class ComboBoxCloseEvent implements IDwcEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
