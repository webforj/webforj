package org.dwcj.controls.textcombobox.events;
import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class TextComboBoxCloseEvent implements IDwcEvent{
    
    private final TextComboBox control;

    public TextComboBoxCloseEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }


}
