package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ComboBox;

public class ChoiceBoxCloseEvent implements ComponentEvent{
    
    private final ComboBox control;

    public ChoiceBoxCloseEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
