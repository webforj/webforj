package org.dwcj.events.textComboBox;

import org.dwcj.controls.TextComboBox;
import org.dwcj.events.IDwcEvent;

public class TextComboBoxOpenEvent implements IDwcEvent{
    
    private final TextComboBox control;

    public TextComboBoxOpenEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }

}
