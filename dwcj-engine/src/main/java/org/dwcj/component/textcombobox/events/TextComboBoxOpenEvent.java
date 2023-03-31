package org.dwcj.component.textcombobox.events;

import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.interfaces.ControlEvent;

public class TextComboBoxOpenEvent implements ControlEvent{
    
    private final TextComboBox control;

    public TextComboBoxOpenEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }

}
