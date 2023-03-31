package org.dwcj.component.choicebox.events;

import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ComboBoxCloseEvent implements ControlEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
