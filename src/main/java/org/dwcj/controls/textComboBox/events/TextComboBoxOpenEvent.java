package org.dwcj.controls.textComboBox.events;

import org.dwcj.controls.textComboBox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class TextComboBoxOpenEvent implements IDwcEvent{
    
    private final TextComboBox control;

    public TextComboBoxOpenEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }

}
