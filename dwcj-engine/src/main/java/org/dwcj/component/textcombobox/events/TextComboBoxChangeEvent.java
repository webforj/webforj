package org.dwcj.component.textcombobox.events;

import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.interfaces.ComponentEvent;


public class TextComboBoxChangeEvent implements ComponentEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
