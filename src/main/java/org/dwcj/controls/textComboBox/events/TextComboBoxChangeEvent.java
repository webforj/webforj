package org.dwcj.controls.textcombobox.events;

import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;


public class TextComboBoxChangeEvent implements IDwcEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
