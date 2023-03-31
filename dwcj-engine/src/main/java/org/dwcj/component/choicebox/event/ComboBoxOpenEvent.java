package org.dwcj.component.choicebox.event;

import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ComboBoxOpenEvent implements ControlEvent{
    
    private final ComboBox control;

    public ComboBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
