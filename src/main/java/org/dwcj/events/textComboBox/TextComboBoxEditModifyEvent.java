package org.dwcj.events.textComboBox;

import org.dwcj.controls.TextComboBox;
import org.dwcj.events.IDwcEvent;


public class TextComboBoxEditModifyEvent implements IDwcEvent{

    private final TextComboBox control;

    public TextComboBoxEditModifyEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
