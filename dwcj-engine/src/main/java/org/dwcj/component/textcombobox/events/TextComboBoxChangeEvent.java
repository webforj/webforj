package org.dwcj.component.textcombobox.events;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.textcombobox.TextComboBox;


public class TextComboBoxChangeEvent implements ComponentEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
