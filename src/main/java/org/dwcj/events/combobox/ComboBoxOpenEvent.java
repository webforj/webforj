package org.dwcj.events.combobox;

import org.dwcj.controls.ComboBox;
import org.dwcj.events.IDwcEvent;

public class ComboBoxOpenEvent implements IDwcEvent{
    
    private final ComboBox control;

    public ComboBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
