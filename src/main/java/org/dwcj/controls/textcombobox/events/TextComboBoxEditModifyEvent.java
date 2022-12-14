package org.dwcj.controls.textcombobox.events;

import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.DwcEvent;


public class TextComboBoxEditModifyEvent implements DwcEvent{

    private final TextComboBox control;

    public TextComboBoxEditModifyEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
