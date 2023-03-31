package org.dwcj.component.combobox.events;

import org.dwcj.component.combobox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ComboBoxOpenEvent implements ControlEvent{
    
    private final ComboBox control;

    public ComboBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
