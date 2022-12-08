package org.dwcj.controls.combobox.events;

import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class ComboBoxCloseEvent implements IDwcEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
