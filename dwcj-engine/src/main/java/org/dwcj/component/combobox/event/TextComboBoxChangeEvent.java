package org.dwcj.component.combobox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.ComboBox;


public class TextComboBoxChangeEvent implements ComponentEvent {
    
    private final ComboBox control;

    public TextComboBoxChangeEvent(ComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
