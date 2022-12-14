package org.dwcj.controls.combobox.events;

import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.interfaces.DwcEvent;

public class ComboBoxCloseEvent implements DwcEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
