package org.dwcj.controls.textcombobox.events;

import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.DwcEvent;


public class TextComboBoxChangeEvent implements DwcEvent {
    
    private final TextComboBox control;

    public TextComboBoxChangeEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
