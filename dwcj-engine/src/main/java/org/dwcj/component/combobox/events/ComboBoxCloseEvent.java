package org.dwcj.component.combobox.events;

import org.dwcj.component.combobox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ComboBoxCloseEvent implements ControlEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
