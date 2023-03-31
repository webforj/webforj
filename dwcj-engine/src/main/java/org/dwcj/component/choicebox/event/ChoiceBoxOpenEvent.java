package org.dwcj.component.choicebox.event;

import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public class ChoiceBoxOpenEvent implements ControlEvent{
    
    private final ComboBox control;

    public ChoiceBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
