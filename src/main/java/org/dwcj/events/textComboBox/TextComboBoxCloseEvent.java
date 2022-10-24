package org.dwcj.events.textComboBox;
import org.dwcj.controls.TextComboBox;
import org.dwcj.events.IDwcEvent;

public class TextComboBoxCloseEvent implements IDwcEvent{
    
    private final TextComboBox control;

    public TextComboBoxCloseEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }


}
