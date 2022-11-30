package org.dwcj.events.combobox;

import org.dwcj.controls.ComboBox;
import org.dwcj.events.IDwcEvent;

public class ComboBoxCloseEvent implements IDwcEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
