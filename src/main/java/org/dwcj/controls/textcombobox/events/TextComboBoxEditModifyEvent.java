package org.dwcj.controls.textcombobox.events;

import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;


public class TextComboBoxEditModifyEvent implements IDwcEvent{

    private final TextComboBox control;

    public TextComboBoxEditModifyEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
