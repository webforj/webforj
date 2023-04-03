package org.dwcj.component.combobox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.ComboBox;


public class ComboBoxChangeEvent implements ComponentEvent {
    
    private final ComboBox control;

    public ComboBoxChangeEvent(ComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
