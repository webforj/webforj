package org.dwcj.events.textComboBox;

import org.dwcj.controls.TextComboBox;
import org.dwcj.events.IDwcEvent;


public class TextComboBoxChangeEvent implements IDwcEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
