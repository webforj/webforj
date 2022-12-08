package org.dwcj.controls.textComboBox.events;

import org.dwcj.controls.textComboBox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;


public class TextComboBoxChangeEvent implements IDwcEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
