package org.dwcj.controls.combobox.events;

import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ComboBoxOpenEvent implements ControlEvent{
    
    private final ComboBox control;

    public ComboBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
