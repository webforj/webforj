package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ComboBox;

public class ChoiceBoxOpenEvent implements ComponentEvent{
    
    private final ComboBox control;

    public ChoiceBoxOpenEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
