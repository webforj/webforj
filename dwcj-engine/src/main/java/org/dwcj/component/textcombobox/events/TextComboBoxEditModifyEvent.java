package org.dwcj.component.textcombobox.events;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.textcombobox.TextComboBox;


public class TextComboBoxEditModifyEvent implements ComponentEvent{

    private final TextComboBox control;

    public TextComboBoxEditModifyEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }
}
